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
import net.tupari.lib.SimpFactory

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

  lazy val tableId = "backendstable"   + SimpFactory.inject[ SimpFactory.UniqueNumber].getOrElse("")
  lazy val lockTableId = "lockstable"   + SimpFactory.inject[ SimpFactory.UniqueNumber].getOrElse("")
  lazy val dateSpanId = "dspan" + SimpFactory.inject[ SimpFactory.UniqueNumber].getOrElse("")
  var refreshOn = false

  override protected def dontCacheRendering: Boolean = true


  def render = {
    debug("render called")

    ".backendstbl" #> <table  > <tbody id={ tableId }>{ getTableContents } </tbody></table> &
      ".backendssql" #> <span>{ backendsSql } </span> &
      ".lockstbl" #>  <table > <tbody id={ lockTableId } >{ getLocksTableContents }</tbody>  </table>  &
      ".lockssql" #>   <span>{ locksSql } </span> &
      ".freshnessdate" #> <span id={ dateSpanId } />  &
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
  }//MyTableCreator

  val backendsSql = "select *, (now() - query_start)::text AS running_time  from pg_stat_activity"

  private def getTableContents: NodeSeq = {
    debug("getTableContents starting")
    val data = Common.getData(backendsSql)
    data match{
      case Right( (keys, oaa) ) =>
        new MyTableCreator(keys, oaa).getTableContents
      case Left(errstr) =>
        <tr><td class="error">errstr</td></tr>
      case _ =>
        <tr><td class="error">code error in { this.getClass }</td></tr>
    }
  }
  /** Note: there appears to be a problem with the postgres jdbc driver that causes this query not to show the relation in some cases.  I'm trying to resolve this. */
  class LocksTableCreator(keys: List[String], data: List[List[Any]], lockedRels: Set[String]) extends TableCreator(keys, data){

    override  protected def getDataNodes(key: String, obj: Any, row: Map[String, Any] ): Seq[Node]={
      <td> {
        key match {
          case "pid" => <a href={ "#" + obj}>{ obj }</a>
//          case "relname" if row("locktype") == "table" =>
//            <a href={ "pgact_page?table="+ obj}> { obj }</a>
          case _ =>
            Option(obj).getOrElse("").toString }
        } </td> %
        { key match {
          case "granted" if (obj == false) =>
            new scala.xml.UnprefixedAttribute ("class", "ungrantedlock",   scala.xml.Null)
          case "granted" if lockedRels.contains( row("relname").asInstanceOf[String] ) && row("granted") == true =>
            new scala.xml.UnprefixedAttribute ("class", "grantedlock",   scala.xml.Null)
          case _ =>
            scala.xml.Null
        }}
    }
  }//class  LocksTableCreator
  /** Problem: the relname subselect only works if the connection is to the same db that pgmon is connected to.
   We need to figure out a way to tell what db number we're connected to so we only do the subquery for our db.
   Beyond that figure out a way to connect to the other dbs to get the relnames out of them. */
  private val locksSql = "SELECT (select relname from pg_catalog.pg_class where pg_catalog.pg_class.oid = relation) as relname, * FROM pg_locks ORDER BY pid, relation;"

  private def getLocksTableContents = {
    Common.getData(locksSql) match{
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
        val lockRelList = oaa collect ( {case MapWithNotGranted(km) if (km("relname") != null) => km("relname").asInstanceOf[String]})
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


