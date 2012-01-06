package net.tupari.pgmon.comet

import _root_.java.util.Date

import _root_.net.liftweb.util._
import _root_.net.liftweb.common.Box
import    _root_.net.liftweb.util.TimeHelpers._
import net.liftweb.common.{Logger, Full}
import net.liftweb.http.js.JsCmds.{Replace, SetHtml}
import xml.{TopScope, NodeSeq, Text}
import net.liftweb.http.{SHtml, S, CometActor}
import net.liftweb.http.js.JsCmds
import scala.xml.Node

object Common{

  //returns an either (tuple, error string)
  def getData(q: String, target_host: String = null, cur_name: String = null): Either[String, Tuple2[ List[String],List[List[Any]] ] ] = {
    try{
      Right(net.liftweb.db.DB.performQuery(q)   )
    }catch{
      //I don't know why but a query that returns no data is considered an error
      case ex: org.postgresql.util.PSQLException  if (ex.getMessage.contains("No results were returned by the query."))   =>
        Right(Nil, Nil)

    }
    //todo: catch exception, return Left
  }


}

class TableCreator(keys: List[String], data: List[List[Any]]) extends Logger{
  //val nodeBuf = new scala.xml.NodeBuffer 

  val keysToIgnore: List[String] = List()

  protected def getHeaderRow( ): Seq[Node] = {
    <tr> { keys.filterNot(key => keysToIgnore.contains(key)).map( key => getHeaderNodes( key ) ) } </tr>
  }
  protected def getHeaderNodes(key: String ): Seq[Node] = {
    <th> { key } </th>
  }
  var rowodd = true
  protected def shouldFlipRowOdd :Boolean = {
    return true
  }
  protected def getDataRow(oa: List[Any]): Node = {
    if (shouldFlipRowOdd)
      rowodd = ! rowodd
    val zip = keys.zip(oa)
    <tr> { (zip map ( (tuple: Tuple2[String,Any] ) => tuple match {
      case (key, v) if ! keysToIgnore.contains(key)  => getDataNodes(key, v, zip.toMap)
      case _ => NodeSeq.Empty
    } )).flatMap(x => x) }</tr> %
      new scala.xml.UnprefixedAttribute ( "class" ,
        if(rowodd){ "RowOdd" } else { "RowEven"} ,
        scala.xml.Null)
  }
  protected def getDataNodes(key: String, obj: Any, row: Map[String, Any] ): Seq[scala.xml.Node] = {
    <td> { obj.toString } </td>
  }

  def getTableContents: NodeSeq = {
    val nodeBuf = new scala.xml.NodeBuffer
    nodeBuf ++=     getHeaderRow( )
    nodeBuf ++= data.map( oa =>  getDataRow(oa))
    //info("getTableContents: returning "+ nodeBuf)
    nodeBuf
  }
}

class PgMonCometSlonyStatusActor  extends CometActor with Logger{

  var tableId = "slonytable"

  override protected def dontCacheRendering: Boolean = true

  def render = {
    <table id={ tableId } > { getTableContents } </table>
  }

  def getTableContents = {
    Common.getData("select schemaname from pg_tables where tablename = 'sl_log_1'") match{
      case Right( (_, Nil)) =>
        <tr><td>No slony installation detected</td></tr>
      case Right( (_, lla) ) =>
        val sql = lla.map( la => la(0) ) map ( schema => "select "+schema+".slonyversion( ) AS version, * from "+schema+".sl_status" ) mkString(" UNION ALL ")
        Common.getData(sql) match{
          case Right( (keys, oaa) ) =>
            <tr> { keys.map( key => <th> { key }</th> ) } </tr> ++
              oaa.map( oa => <tr>  { oa.map( o => <td> { o.toString } </td> ) } </tr>   )
          case Left(errstr) =>
            <tr><td class="error">{errstr}</td></tr>
        }
      case Left(errstr) =>
        <tr><td class="error">{errstr}</td></tr>
    }
  }
  override def lowPriority : PartialFunction[Any, Unit] = {
    case _ =>
      partialUpdate(SetHtml(tableId, getTableContents))
  }

}//class

class PgMonCometBackendsActor  extends CometActor with Logger{

  var tableId = "backendstable"
  val lockTableId = "lockstable"
  var refreshOn = false

  override protected def dontCacheRendering: Boolean = true


  def render = {
    debug("render called")
    //Schedule.schedule(this, "update", 1L)

    ".backendstbl" #> <table  > <tbody id={ tableId }>{ getTableContents } </tbody></table> &
      ".lockstbl" #>  <table > <tbody id={ lockTableId } >{ getLocksTableContents }</tbody>  </table>  &
      ".reloadbox" #>   SHtml.ajaxCheckbox (false, { (b: Boolean) =>
        refreshOn = b
        if (b) this ! "update"
      })

  }

  private val RUNNING_TIME = "running_time"

  class MyTableCreator(keys: List[String], data: List[List[Any]]) extends TableCreator(keys, data){
    override val keysToIgnore = List("running_time")

    override protected def getHeaderNodes(key: String ): Seq[Node] = {
      val ans = new scala.xml.NodeBuffer
      ans ++= <th> { key } </th>
      if (key == "query_start"){
        ans ++= <th>running time</th>
      }
      ans
    }
    override  protected def getDataNodes(key: String, obj: Any, row: Map[String, Any] ): Seq[Node]={
      val ans = new scala.xml.NodeBuffer
      ans ++=  <td> { Option(obj).getOrElse("").toString } </td> %
        { key match {
          case "waiting" if (obj == true) =>
            new scala.xml.UnprefixedAttribute ("bgcolor", "red",   scala.xml.Null)
          case _ =>
            scala.xml.Null
        } }
      lazy val sql = row("current_query");
      if (key == "query_start"){
        if (sql == null || sql == "<IDLE>")
          ans ++= <td></td>
        else
          ans ++= <td>{ row("running_time") } </td>
      }
      ans
    }
  }//class
  def getTableContents: NodeSeq = {
    info("getTableContents starting")
    val data = Common.getData("select *, (now() - query_start)::text AS running_time  from pg_stat_activity ")
    data match{
      case Right( (keys, oaa) ) =>
        new MyTableCreator(keys, oaa).getTableContents
      case Left(errstr) =>
        <tr><td class="error">errstr</td></tr>
      case _ =>
        <tr><td class="error">code error in { this.getClass }</td></tr>
    }
  }

  class LocksTableCreator(keys: List[String], data: List[List[Any]], lockedRels: Set[String]) extends TableCreator(keys, data){

    override  protected def getDataNodes(key: String, obj: Any, row: Map[String, Any] ): Seq[Node]={
      <td> {
        key match {
          case "pid" => <a href={ "#" + obj}>{ obj }</a>
          case "relname" if row("locktype") == "table" =>
            <a href={ "pgact.jsp?table="+ obj}> { obj }</a>
          case _ =>
            Option(obj).getOrElse("").toString }
        } </td> %
        { key match {
          case "granted" if (obj == false) =>
            new scala.xml.UnprefixedAttribute ("bgcolor", "red",   scala.xml.Null)
          case "relname" if lockedRels.contains( row("relname").asInstanceOf[String] ) && row("granted") == true =>
            new scala.xml.UnprefixedAttribute ("bgcolor", "lime",   scala.xml.Null)
          case _ =>
            scala.xml.Null
        }
        }
    }
  }//class

  def getLocksTableContents = {
    Common.getData("SELECT (select relname from pg_catalog.pg_class where pg_catalog.pg_class.oid = relation) as relname, * FROM pg_locks ORDER BY pid, relation;") match{
      case Right( (keys, oaa) ) =>
        object MapWithNotGranted{
          def unapply(list: List[_]) = {
            val zip = keys.zip(list).toMap
            if (zip("granted") == false)
              Option(zip)
            else
              None
          }
        }
        //doesn't work:
        //val lockedRelations: Set[String] = oaa2 collect ( {case MapWithNotGranted(km) => km("relation").asInstanceOf[String]}).toSet
        //error: missing parameter type for expanded function The argument types of an anonymous function must be fully known. (SLS 8.5)
        val lockRelList = oaa collect ( {case MapWithNotGranted(km) => km("relname").asInstanceOf[String]})
        val lockedRelations = lockRelList.toSet

        new LocksTableCreator(keys, oaa, lockedRelations).getTableContents

      case Left(errstr) =>
        <tr><td class="error">errstr</td></tr>
      case _ =>
        <tr><td class="error">code error in { this.getClass }</td></tr>
    }
  }
  override def lowPriority : PartialFunction[Any, Unit] = {
    case "update" =>
      partialUpdate(SetHtml(tableId, getTableContents))
      partialUpdate(SetHtml(lockTableId, getLocksTableContents))
      if (refreshOn  )
        Schedule.schedule(this, "update", 2500L)

  }
}//class


