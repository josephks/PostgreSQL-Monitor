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
 * To change this template use File | Settings | File Templates.
 */

object SchemaPrinter{
  val headers =     ("text/html" -> "charset=UTF-8") :: Nil
  /** Use this method in Boot to construct the api */
  def getResponse(formatterCode:Option[String]) = {
    val r = new java.io.PipedInputStream()
    val w = new java.io.PrintWriter(new java.io.PipedOutputStream(r))
    scala.actors.Actor.actor({
      new DbInspector().getDbSchema(w)
    })
    Full(new StreamingResponse(r, () => {}, -1, headers, Nil, 200))
    }   // final case class StreamingResponse(data: {def read(buf: Array[Byte]): Int}, onEnd: () => Unit, size: Long, headers:  List[(String, String)], cookies: List[HTTPCookie], code: Int)

}
  object OutputType extends Enumeration {
     type OutputType = Value
     val Lift, JPA, Text, HTML = Value
   }
   object Constraint extends Enumeration {
     type Constraint = Value
     val Pkey, Unique, ForeignKey = Value
   }

/** FieldDef (represents a field in a table) needs to be declared outside TableDef, since
 * tables reference fields from other tables. FieldDefs are stored inside TableDefs but don't reference back to them */
class FieldDef(name:String,  fieldType:String, notnull:Boolean, indexed:Boolean  ){
  class ForeignKeySingleRef(other: FieldDef)
  var  constraints: List[Constraint.Constraint] = Nil
  var foreignKeys:List[ForeignKeySingleRef]  = Nil
}

class TableDef (name:String){
  class IndexDef(fields:List[FieldDef],  unique:Boolean)
  var fields:List[FieldDef] = Nil
  /** fields indexed by field number */
  val fieldDefMap = scala.collection.mutable.Map[Int, FieldDef]()
  var indexes:List[IndexDef]  = Nil
  var foreignKeys:List[ForeignKeyManyRef] = Nil
  class ForeignKeyManyRef(from: List[FieldDef], to:  List[FieldDef])
}

class DbInspector(outputType: OutputType.OutputType = OutputType.Text) extends LazyLoggable {

  private var tableDefs = Map[String, TableDef]()
  private var tableDefsByOid = Map[Long, TableDef]()
  private var formatterCode:Option[String] = None

  //these fields  are used during inital data processing
  private var schemaList:(List[String], List[List[Any]]) = null

  private lazy val oidIdx = schemaList match { case (keys, _) => keys.indexOf("oid")  }
  private lazy val schemaNameIdx = schemaList match { case (keys, _) => keys.indexOf("schema")  }
  private var constraintData:(List[String], List[List[Any]]) = null
  private var indexData:(List[String], List[List[Any]]) = null
  
  private def getTableOid(name:String ) = schemaList  match { case (_, oaa) => oaa.find (oa => oa(schemaNameIdx) == name).map( oa => oa(oidIdx).asInstanceOf[Long] ) }
  

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

  def getDbSchema(writer:PrintWriter ) = {
    val sql = "select * from ( select (select nspname FROM pg_catalog.pg_namespace where oid = relnamespace) AS schema," +
      " relname, c.relkind, oid from pg_catalog.pg_class c WHERE c.relkind in ('v','r')) aa where schema = 'public'  ORDER BY schema, relname;"
    Common.getData(sql)   match{
      case Right( (keys, oaa) ) =>
        schemaList = (keys, oaa)
        logger.info("query returned "+oaa.size+" rows")
        getTableSchemaData()
        //we now have a list of tables and views
        oaa.foreach( {list:List[Any] =>
          val map = keys.zip(list).toMap
          val oid = map("oid").asInstanceOf[Long]
          map("relkind") match {
            case "r" => getTableSchema(writer, map)
            case "v" => //todo: handle view
          }  }
        )
      case Left(errstr) =>
        writer.print(errstr)
    }
    writer.flush()
    writer.close()
  }
  var tableSchemaData:Map[String,  List[Map[String, Any]]]  = null
  
  private def getTableSchemaData(){
           getIndexAndConstraintData()
      val sql = "SELECT   a.attrelid,  a.attname,  pg_catalog.format_type(a.atttypid, a.atttypmod)," +
        "(SELECT substring(pg_catalog.pg_get_expr(d.adbin, d.adrelid) for 128) " +
        " FROM pg_catalog.pg_attrdef d WHERE d.adrelid = a.attrelid AND d.adnum = a.attnum AND a.atthasdef)," +
        " a.attnotnull, a.attnum, (SELECT c.collname FROM pg_catalog.pg_collation c, pg_catalog.pg_type t  " +
        " WHERE c.oid = a.attcollation AND t.oid = a.atttypid AND a.attcollation <> t.typcollation) AS attcollation " +
        "FROM pg_catalog.pg_attribute a   WHERE  a.attnum > 0 AND NOT a.attisdropped ORDER BY a.attrelid, a.attnum;"

    Common.getData(sql)   match{
      case Right( (keys, oaa) ) =>
        tableSchemaData = oaa groupBy ( oa => getTableName(oa(0)).getOrElse(null)) mapValues  (  oaa => oaa.map(oa => keys.zip(oa).toMap))
        //                                                                       ^ list of string->oaa   mV-> string->List[Map]
        tableSchemaData.foreach{ case (tabname, listOfMaps) => {
          val tabDef = new TableDef(tabname)
          getTableOid(tabname).map{ tableOid =>
            val fieldDefMap = tabDef.fieldDefMap //DELETE = scala.collection.mutable.Map[Int, tabDef.FieldDef]()
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
                map("indkey").asInstanceOf[List[Int]] match{
                  case fieldIdArr if fieldIdArr.length > 1 && map("indrelid") == tableOid =>
                    tabDef.indexes = new tabDef.IndexDef( fieldIdArr.map(fid => fieldDefMap(fid) ) , map("indisunique").asInstanceOf[Boolean] ) :: tabDef.indexes
                  //can also check if for primary key
                  case _ =>
                    //todo: add multi-field indexes
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
              case map if map("conrelid") != null && 'f' == map("contype")  =>
              val fromTableOid = map("conrelid").asInstanceOf[Long]
              val otherTableOid = map("conindid").asInstanceOf[Long]
              val myCols = map("conkey").asInstanceOf[Array[Int]]
              val refCols = map("confkey").asInstanceOf[Array[Int]]
              if (myCols.length == 1) {
                for ( toTabDef <- tableDefsByOid.get(otherTableOid) ;
                      fromTabDef <- tableDefsByOid.get(fromTableOid) ;
                      fromField <- fromTabDef.fieldDefMap.get(myCols(0))  ;
                      toField <- toTabDef.fieldDefMap.get(refCols(0)) ) {
                  new fromField.ForeignKeySingleRef(toField) :: fromField.foreignKeys
                }
              } else{
                  for ( toTabDef <- tableDefsByOid.get(otherTableOid) ;
                      fromTabDef <- tableDefsByOid.get(fromTableOid) ){
                    new  fromTabDef.ForeignKeyManyRef( myCols.map( fromTabDef.fieldDefMap(_) ).toList, refCols.map( fromTabDef.fieldDefMap(_) ).toList ) :: fromTabDef.foreignKeys
                  }
              }
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
    val indexSql = "SELECT * FROM pg_index"
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
          case map if map("conrelid") == tableoid && new StringOps("pu").contains( map("contype").asInstanceOf[Char])  =>
            map("contype") match { // c = check constraint, f = foreign key constraint, p = primary key constraint, u = unique constraint, t = constraint trigger, x = exclusion constraint
              case 'p' => Constraint.Pkey
              case 'u' => Constraint.Unique
              //case 'f' => null
            }
        }
    }
  }
  def getTableSchema(writer:PrintWriter, map:Map[String,Any]){  //old, delete
    val oid = map("oid")
    val tablename = map("relname")
    val oldsql =  "SELECT a.attname,          pg_catalog.format_type(a.atttypid, a.atttypmod)," +
      "(SELECT substring(pg_catalog.pg_get_expr(d.adbin, d.adrelid) for 128)   " +
      "         FROM pg_catalog.pg_attrdef d     " +
      "       WHERE d.adrelid = a.attrelid AND d.adnum = a.attnum AND a.atthasdef),           a.attnotnull, a.attnum," +
      "           (SELECT c.collname FROM pg_catalog.pg_collation c, pg_catalog.pg_type t"+
        "   WHERE c.oid = a.attcollation AND t.oid = a.atttypid AND a.attcollation <> t.typcollation)" +
      " AS attcollation FROM pg_catalog.pg_attribute a   WHERE a.attrelid = '" + oid +"' AND a.attnum > 0 AND NOT a.attisdropped "  +
        "ORDER BY a.attnum; "
    /*   gives output like:
    attname     |         format_type         |                         ?column?                          | attnotnull | attnum | attcollation
----------------+-----------------------------+-----------------------------------------------------------+------------+--------+--------------
 userkey        | integer                     | nextval(('public.directory_userkey_seq'::text)::regclass) | t          |      1 |
 username       | text                        |                                                           | t          |      2 |
 lastmod        | timestamp without time zone | ('now'::text)::timestamp(6) with time zone                | t          |      3 |
 passwd         | character(40)               |                                                           | f          |      4 |

     */
    /* next step: get index def. Gives unique fields & primary key. This query isn't good enough, we need to get the actual field names
         local]:owl=> SELECT c2.relname, i.indisprimary, i.indisunique, i.indisclustered, i.indisvalid, pg_catalog.pg_get_indexdef(i.indexrelid, 0, true),
owl->           pg_catalog.pg_get_constraintdef(con.oid, true), contype, condeferrable, condeferred, c2.reltablespace
owl->         FROM pg_catalog.pg_class c, pg_catalog.pg_class c2, pg_catalog.pg_index i
owl->           LEFT JOIN pg_catalog.pg_constraint con ON (conrelid = i.indrelid AND conindid = i.indexrelid AND contype IN ('p','u','x'))
owl->         WHERE c.oid = '16548' AND c.oid = i.indrelid AND i.indexrelid = c2.oid
owl->         ORDER BY i.indisprimary DESC, i.indisunique DESC, c2.relname
owl-> ;
             relname              | indisprimary | indisunique | indisclustered | indisvalid |                                                        pg_get_indexdef                                                         | pg_get_const
raintdef  | contype | condeferrable | condeferred | reltablespace
----------------------------------+--------------+-------------+----------------+------------+--------------------------------------------------------------------------------------------------------------------------------+-------------
----------+---------+---------------+-------------+---------------
 directory_pkey                   | t            | t           | f              | t          | CREATE UNIQUE INDEX directory_pkey ON directory USING btree (userkey)                                                          | PRIMARY KEY
(userkey) | p       | f             | f           |             0
 directory_lower_username_seg_key | f            | t           | f              | t          | CREATE UNIQUE INDEX directory_lower_username_seg_key ON directory USING btree (lower(username) text_pattern_ops, seg)          |
          |         |               |             |             0
 directory_seg_xid_idx            | f            | t           | f              | t          | CREATE UNIQUE INDEX directory_seg_xid_idx ON directory USING btree (seg, xid) WHERE xid IS NOT NULL                            |
          |         |               |             |             0
 directory_flow_idx               | f            | f           | f              | t          | CREATE INDEX directory_flow_idx ON directory USING btree (seg, statchangedate) WHERE status = ANY (ARRAY[4, 6])                |
          |         |               |             |             0
 directory_nonactive_idx          | f            | f           | f              | t          | CREATE INDEX directory_nonactive_idx ON directory USING btree (seg) WHERE status = 4                                           |
          |         |               |             |             0
 directory_seg_lower_username_idx | f            | f           | f              | t          | CREATE INDEX directory_seg_lower_username_idx ON directory USING btree (seg, lower(username))                                  |
          |         |               |             |             0
 directory_seg_segadm_idx         | f            | f           | f              | t          | CREATE INDEX directory_seg_segadm_idx ON directory USING btree (seg, segadm) WHERE segadm IS NOT NULL AND segadm > 0::smallint |
          |         |               |             |             0
(7 rows)
      */
    def toHtml(sql:String )= {
    Common.getData(sql)   match{
      case Right( (keys, oaa) ) =>
        var tc = new TableCreator(keys, oaa)
        writer.write {
          tc.getTable(Some("table "+map("relname"))).toString()
        }
      case Left(errstr) =>
        writer.write(errstr)
    }    }
    toHtml(oldsql)
      
       val constraintSql = " SELECT conname,  pg_catalog.pg_get_constraintdef(r.oid, true) as condef " +
         " FROM pg_catalog.pg_constraint r WHERE r.conrelid = '" + oid +" ' AND r.contype = 'f' ORDER BY 1 "
       toHtml(constraintSql)
    
    
       val forKeySql  = "SELECT conname, conrelid::pg_catalog.regclass, pg_catalog.pg_get_constraintdef(c.oid, true) as condef " +
   " FROM pg_catalog.pg_constraint c WHERE c.confrelid = '" + oid +"' AND c.contype = 'f' ORDER BY 1   "
    toHtml(forKeySql)
       /*        conname        |  conrelid   |                              condef
----------------------+-------------+-------------------------------------------------------------------
 $1                   | bounced     | FOREIGN KEY (uid) REFERENCES directory(userkey) ON DELETE CASCADE
 dirtydcache_uid_fkey | dirtydcache | FOREIGN KEY (uid) REFERENCES directory(userkey) ON DELETE CASCADE
(2 rows)            */
          //I see no use for this now
        val triggerSql = """SELECT t.tgname, pg_catalog.pg_get_triggerdef(t.oid, true), t.tgenabled
        FROM pg_catalog.pg_trigger t
        WHERE t.tgrelid = '16548' AND NOT t.tgisinternal
        ORDER BY 1   """
             toHtml(triggerSql)
    writer.println("<hr />")
             /*
             get parent child relationships:
               LOG:  statement: SELECT c.oid::pg_catalog.regclass FROM pg_catalog.pg_class c, pg_catalog.pg_inherits i WHERE c.oid=i.inhparent AND i.inhrelid = '16548' ORDER BY inhseqno
LOG:  statement: SELECT c.oid::pg_catalog.regclass FROM pg_catalog.pg_class c, pg_catalog.pg_inherits i WHERE c.oid=i.inhrelid AND i.inhparent = '16548' ORDER BY c.oid::pg_catalog.regclass::pg_catalog.text;


              */


  }

}