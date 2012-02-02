package net.tupari.pgmon.schemagen

import net.tupari.pgmon.comet.Common
import net.liftweb.http.StreamingResponse
import java.io._
import net.tupari.pgmon.lib.TableCreator
import net.liftweb.common.{LazyLoggable, Full}
import collection.immutable.StringOps

/**
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 1/18/12
 * Time: 10:18 PM
 */

object SchemaPrinter{
  val headers =     ("text/html" -> "charset=UTF-8") :: Nil
  /** Use this method in Boot to construct the api */
  def getResponse(formatterCode:Option[String]) = {
    val r = new java.io.PipedInputStream()
    val w = new java.io.PrintWriter(new java.io.PipedOutputStream(r))
    scala.actors.Actor.actor({   //spin off thread to write to the writer
      val schemaFormatter = formatterCode match{
        case Some("jpa") => Some(new JpaFormatter(w))
        case Some("lift") => Some(new LiftFormatter(w))
        case _ => Some(new TableLister(w))
      }
      try{
        new DbInspector().getDbSchema(w, schemaFormatter)
      }finally{
        w.close()
      }
    })
    Full(new StreamingResponse(r, () => {}, -1, headers, Nil, 200))
    // final case class StreamingResponse(data: {def read(buf: Array[Byte]): Int}, onEnd: () => Unit, size: Long, headers:  List[(String, String)], cookies: List[HTTPCookie], code: Int)
  }
}
/** Different constraint types */
object Constraint extends Enumeration {
  type Constraint = Value
  val Pkey, Unique, ForeignKey = Value
}

/** FieldDef (represents a field in a table). Needs to be declared outside TableDef, since
 * tables reference fields from other tables. FieldDefs are stored inside TableDefs but don't reference back to them */
class FieldDef(val name:String, val fieldType:String,val notnull:Boolean,val indexed:Boolean  ){
  class ForeignKeySingleRef(othertable:TableDef , other: FieldDef)
  var constraints: List[Constraint.Constraint] = Nil
  var foreignKeys:List[ForeignKeySingleRef]  = Nil
}

  class ForeignKeyManyRef(val fromtable: TableDef, val from: List[FieldDef], val fromUniq: Boolean, //is the from side unique?
                          val totable: TableDef, val to: List[FieldDef], val toUniq: Boolean)

class TableDef (val name:String){
  class IndexDef(val fields:List[FieldDef],val unique:Boolean, val primary:Boolean)
  var fields:List[FieldDef] = Nil
  /** fields indexed by field number */
  val fieldDefMap = scala.collection.mutable.Map[Int, FieldDef]()
  var indexes:List[IndexDef]  = Nil
  var incomingForeignKeys:List[ForeignKeyManyRef] = Nil
  var outgoingForeignKeys:List[ForeignKeyManyRef] = Nil
  def getPkField = {
    fields.find{ f => f.constraints.contains(Constraint.Pkey) }
  }
  def isUniqIndexOn(fieldnames: List[String]) = {
    indexes.exists ( indexDef =>
      indexDef.unique &&
        indexDef.fields.length == fieldnames.length
        && fieldnames.intersect( indexDef.fields.map(f => f.name) ).length == fieldnames.length)
  }
  override def toString = "TableDef("+name+")"
}

class DbInspector() extends LazyLoggable {
  private var tableDefs = Map[String, TableDef]()
  private var tableDefsByOid = Map[Long, TableDef]()

  //these fields  are used during inital data processing
  private var schemaList:(List[String], List[List[Any]]) = null

  private lazy val oidIdx = schemaList match { case (keys, _) => keys.indexOf("oid")  }  //index of the oid column
  //private lazy val schemaNameIdx = schemaList match { case (keys, _) => keys.indexOf("schema")  }
  private lazy val relNameIdx = schemaList match { case (keys, _) => keys.indexOf("relname")  }  //index of the relname column
  private var constraintData:(List[String], List[List[Any]]) = null
  private var indexData:(List[String], List[List[Any]]) = null

  /** Given a table name return oid */
  private def getTableOid(name:String): Option[Long] = {
    schemaList  match {
      case (keys, oaa) => oaa.find ({oa =>
        oa(relNameIdx) == name
      }).
        map( oa => Some(oa(oidIdx).asInstanceOf[Long]) ).getOrElse({ logger.warn("could not find oid for table "+name+" in "+oaa.map(oa => keys.zip(oa).toMap)) ; None})
    } //match
  }

  private def getTableName(oid:Any): Option[String] = {
    schemaList match {
      case (keys, oaa) =>
        for(oa <- oaa){
          if (oa(oidIdx) == oid)
            return Some(oa(keys.indexOf("relname")).asInstanceOf[String])
        }
        return None
    }
  }

  def getDbSchema(writer:PrintWriter, schemaFormatter: Option[SchemaFormatter]) = {
    val sql = "select * from ( select (select nspname FROM pg_catalog.pg_namespace where oid = relnamespace) AS schema," +
      " relname, c.relkind, oid from pg_catalog.pg_class c WHERE c.relkind in ('v','r')) aa where schema = 'public'  ORDER BY schema, relname;"
    Common.getData(sql)   match{
      case Right( (keys, oaa) ) =>
        schemaList = (keys, oaa)
        logger.info("query returned "+oaa.size+" rows, schemaFormatter: "+schemaFormatter)
        getTableSchemaData()
        //we now have a list of tables and views
        schemaFormatter.foreach{sf =>
          logger.info("about to process "+ tableDefs.values)
          tableDefs.values.foreach{ td =>
            logger.info("passing table "+td.name+" to formatter")
            sf.format(td)
          }
        }
      case Left(errstr) =>
        writer.print(errstr)
    }
    writer.flush()
    writer.close()
  }
  /** Keyed by table name, a list of field info */
  private var tableSchemaData:Map[String,  List[Map[String, Any]]]  = null

  private def getTableSchemaData(){
    getIndexAndConstraintData()
    //see bottom of file for example of what output of this sql looks like
    val sql = "SELECT a.attrelid,  a.attname,  pg_catalog.format_type(a.atttypid, a.atttypmod)," +
      "(SELECT substring(pg_catalog.pg_get_expr(d.adbin, d.adrelid) for 128) " +
      " FROM pg_catalog.pg_attrdef d WHERE d.adrelid = a.attrelid AND d.adnum = a.attnum AND a.atthasdef)," +
      " a.attnotnull, a.attnum, (SELECT c.collname FROM pg_catalog.pg_collation c, pg_catalog.pg_type t  " +
      " WHERE c.oid = a.attcollation AND t.oid = a.atttypid AND a.attcollation <> t.typcollation) AS attcollation " +
      "FROM pg_catalog.pg_attribute a WHERE  a.attnum > 0 AND NOT a.attisdropped ORDER BY a.attrelid, a.attnum;"

    Common.getData(sql)   match{
      case Right( (keys, oaa) ) =>
        tableSchemaData = oaa.groupBy( oa => getTableName(oa(0)).orNull).mapValues(  oaa => oaa.map(oa => keys.zip(oa).toMap))
        //                                                                       ^ list of string->oaa   mV-> string->List[Map]
        tableSchemaData.foreach{ case (tabname, listOfMaps) => {
          //logger.info("  getTableSchemaData() processing for "+tabname);
          val tabDef = new TableDef(tabname)
          getTableOid(tabname).map{ tableOid =>
            logger.info("  getTableSchemaData() processing for "+tabname+" (oid "+tableOid+")");
            val fieldDefMap = tabDef.fieldDefMap
            tabDef.fields = listOfMaps.map{ map =>
              val attributeNum = map("attnum").asInstanceOf[Number].intValue()
              val constraints = getFieldConstraints(tableOid, attributeNum )
              val hasIndex =  indexData match{
                case (keys, oaa) =>
                  oaa.exists{oa =>
                    val map = keys.zip(oa).toMap
                    map("indrelid") == tableOid && map("indkey") ==  Array(attributeNum)
                  }
                case _ => false
              }
              val fielddef = new FieldDef( map("attname").asInstanceOf[String], map("format_type").asInstanceOf[String], map("attnotnull").asInstanceOf[Boolean],  hasIndex  )
              fieldDefMap += (attributeNum -> fielddef)
              fielddef
            } // tabDef.fields = listOfMaps.map{ map =>
            //now add index data
            indexData match { case (keys, oaa) =>
              oaa.foreach{ oa =>
                val map = keys.zip(oa).toMap
                val indkey = map("indkey")
                indkey.asInstanceOf[java.sql.Array].getArray.asInstanceOf[Array[Any]].toList match{
                  case fieldIdArr if fieldIdArr.length >= 1 && map("indrelid") == tableOid =>
                    val isUniq = map("indisunique").asInstanceOf[Boolean]
                    val isPrimary = map("indisprimary").asInstanceOf[Boolean]
                    tabDef.indexes = new tabDef.IndexDef( fieldIdArr.map(fid => fieldDefMap(fid.asInstanceOf[Int]) ) , isUniq, isPrimary ) :: tabDef.indexes

                     if (fieldIdArr.length == 1) {
                       val field = fieldDefMap(fieldIdArr(0).asInstanceOf[Int])
                       if (isUniq) field.constraints =  Constraint.Unique :: field.constraints
                       if (isPrimary) field.constraints =  Constraint.Pkey :: field.constraints
                            }
                  case _ =>
                }
              }
            }
            tableDefs += (tabname -> tabDef)
            tableDefsByOid += ( tableOid -> tabDef)
          }
        }} //foreach
        //now we have data for all tables, hook up the foreign keys
        constraintData match{
          case (keys, oaa) =>
            oaa.map(oa => keys.zip(oa).toMap ).foreach{
              case map if map("conrelid") != null && "f" == map("contype")  =>
                val fromTableOid = map("conrelid").asInstanceOf[Long]
                val otherTableOid = map("confrelid").asInstanceOf[Long]
                logger.info("procing fkey from "+fromTableOid+" "+tableDefsByOid(fromTableOid)+" to " + otherTableOid+" "+ tableDefsByOid.get(otherTableOid))
                val fromCols = map("conkey").asInstanceOf[java.sql.Array].getArray.asInstanceOf[Array[java.lang.Integer]] //asInstanceOf[Array[Int]]
                val refCols = map("confkey").asInstanceOf[java.sql.Array].getArray.asInstanceOf[Array[java.lang.Integer]]  //asInstanceOf[Array[Int]]
                  for ( toTabDef <- tableDefsByOid.get(otherTableOid) ;
                        fromTabDef <- tableDefsByOid.get(fromTableOid) ){
                    val fromFields = fromCols.map(fromTabDef.fieldDefMap(_)).toList
                    val toFields = refCols.map(toTabDef.fieldDefMap(_)).toList
                    val fkey = new ForeignKeyManyRef(fromTabDef , fromFields, fromTabDef.isUniqIndexOn(fromFields.map(f => f.name)), //is the from side unique?
                                    toTabDef, toFields, toTabDef.isUniqIndexOn(toFields.map(f => f.name)) )
                    fromTabDef.outgoingForeignKeys = fkey :: fromTabDef.outgoingForeignKeys
                    toTabDef.incomingForeignKeys = fkey :: fromTabDef.outgoingForeignKeys
                  }
              case _ =>
            }
        }
      case Left(errstr) =>
        logger.error(errstr)
    }
  }
  private def getIndexAndConstraintData(){    // http://www.postgresql.org/docs/9.1/interactive/catalog-pg-constraint.html
    val sql = "SELECT * FROM pg_constraint"
     Common.getData(sql)   match{
      case Right( (keys, oaa) ) =>
           constraintData =(keys, oaa)
      case Left(errstr) =>
        logger.error(errstr)
    }
    //cast indkey to int[] (otherwise ends up being PgObject). This second indkey column ends up in the map
    val indexSql = "SELECT *, indkey::int[] FROM pg_index"
    Common.getData(indexSql)   match{
      case Right( (keys, oaa) ) =>
           indexData =(keys, oaa)
      case Left(errstr) =>
        logger.error(errstr)
    }
  }
  private def getFieldConstraints(tableoid:Any, fieldNum:Int ) = {  //gets uniq and primary key constraints
    constraintData    match{
      case (keys, oaa) =>
        oaa.map(oa => keys.zip(oa).toMap ).collect{
          case map if map("conrelid") == tableoid && new StringOps("pu").contains( map("contype").asInstanceOf[String])  => //is string, not char
            map("contype") match { // c = check constraint, f = foreign key constraint, p = primary key constraint, u = unique constraint, t = constraint trigger, x = exclusion constraint
              case "p" => Constraint.Pkey
              case "u" => Constraint.Unique
              //case 'f' => null
            }
        }
    }
  }
}
    /*
    attname     |         format_type         |                         ?column?                          | attnotnull | attnum | attcollation
----------------+-----------------------------+-----------------------------------------------------------+------------+--------+--------------
 userkey        | integer                     | nextval(('public.directory_userkey_seq'::text)::regclass) | t          |      1 |
 username       | text                        |                                                           | t          |      2 |
 lastmod        | timestamp without time zone | ('now'::text)::timestamp(6) with time zone                | t          |      3 |
 passwd         | character(40)               |                                                           | f          |      4 |

     */
