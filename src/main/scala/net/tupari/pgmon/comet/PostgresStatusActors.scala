package net.tupari.pgmon.comet

import _root_.java.util.Date

import _root_.net.liftweb.util._
import _root_.net.liftweb.common.Box
import    _root_.net.liftweb.util.TimeHelpers._
import net.liftweb.common.{Logger, Full}
import xml.{TopScope, NodeSeq, Text}
import net.liftweb.http.{SHtml, S, CometActor}
import net.liftweb.http.js.JsCmds
import scala.xml.Node
import net.tupari.lib.SimpFactory
import net.tupari.pgmon.lib.TableCreator
import net.liftweb.http.js.JsCmds.{After, Replace, SetHtml}
import net.liftweb.http.js.JE.JsRaw

class PgMonCometSlonyStatusActor  extends CometActor with Logger{

  private val slonySql = "select schemaname from pg_tables where tablename = 'sl_log_1'"
  private var sqlUsed = ""
  private var tableId = "slonytable" + SimpFactory.inject[SimpFactory.UniqueNumber].get
  private var sqlSpanId = "slonysqlspan"  + SimpFactory.inject[SimpFactory.UniqueNumber].get
  private var refreshOn = false

  override protected def dontCacheRendering: Boolean = true

  def render = {
    ".slonytbl" #> <table  > <tbody id={ tableId }>{ getTableContents } </tbody></table> &
      ".backendssql" #> <span  id={ sqlSpanId }>{ sqlUsed } </span> &
      ".reloadbox" #>   SHtml.ajaxCheckbox (false, { (b: Boolean) =>
        refreshOn = b
        if (b) this ! "update"
      })
  }

  def getTableContents = {
    Common.getData(slonySql) match{
      case Right( (_, Nil)) =>
        <tr><td>No slony installation detected</td></tr>
      case Right( (_, lla) ) =>
        val sql = lla.map( la => la(0) ) map ( schema => "select "+schema+".slonyversion( ) AS version, * from "+schema+".sl_status" ) mkString(" UNION ALL ")
        sqlUsed = sql
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
    case "update" =>
      partialUpdate(SetHtml(tableId, getTableContents))
      partialUpdate(SetHtml(sqlSpanId, Text(sqlUsed))) //in practice doesn't change much
      if (refreshOn  )
        Schedule.schedule(this, "update", 2500L)
  }

}//class

class PgMonCometBackendsActor  extends CometActor with Logger{

  private lazy val tableId = "backendstable"   + SimpFactory.inject[ SimpFactory.UniqueNumber].getOrElse("")
  private lazy val lockTableId = "lockstable"   + SimpFactory.inject[ SimpFactory.UniqueNumber].getOrElse("")
  private lazy val dateSpanId = "dspan" + SimpFactory.inject[ SimpFactory.UniqueNumber].getOrElse("")
  private var hasDate = false
  private var refreshOn = false

  override protected def dontCacheRendering: Boolean = true


  private lazy val renderOnce = {
    debug("renderOnce called")

    ".backendstbl" #> <table> <tbody id={ tableId }>{ getTableContents } </tbody></table> &
      ".backendssql" #> <span>{ backendsSql } </span> &
      ".lockstbl" #>  <table> <tbody id={ lockTableId } >{ getLocksTableContents }</tbody>  </table>  &
      ".lockssql" #>   <span>{ locksSql } </span> &
      ".freshnessdate" #> { hasDate = true ;  <span id={ dateSpanId } /> } &
      ".reloadbox" #>   SHtml.ajaxCheckbox (false, { (b: Boolean) =>
        refreshOn = b
        if (b) this ! "update"
      })
  }
  def render = renderOnce

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
      ans ++=  <td> { getString(obj) } </td> %
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
        <tr><td class="error">{ errstr }</td></tr>
      case _ =>
        <tr><td class="error">code error in { this.getClass }</td></tr>
    }
  }

  class LocksTableCreator(keys: List[String], data: List[List[Any]], lockedRels: Set[String]) extends TableCreator(keys, data){

    override  protected def getDataNodes(key: String, obj: Any, row: Map[String, Any] ): Seq[Node]={
      <td> {
        key match {
          case "pid" => <a href={ "#" + obj}>{ obj }</a>
//          case "relname" if row("locktype") == "table" =>
//            <a href={ "pgact_page?table="+ obj}> { obj }</a>
          case _ => getString(obj)
        }
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
        <tr><td class="error">{ errstr }</td></tr>
      case _ =>
        <tr><td class="error">code error in { this.getClass }</td></tr>
    }
  }
  override def lowPriority : PartialFunction[Any, Unit] = {
    case "update" =>
      partialUpdate(SetHtml(tableId, getTableContents))
      partialUpdate(SetHtml(lockTableId, getLocksTableContents))
      if (hasDate)
          partialUpdate(SetHtml(dateSpanId, Text(now.toString) ))
      if (refreshOn  ) {
        //server side: Schedule.schedule(this, "update", 2500L)
        //client side, better:
        //partialUpdate( After(2500 millis, { this ! "update" ;  net.liftweb.http.js.JsCmds.Noop  }) )
        partialUpdate( After(2500 millis,{jsonSend("Hello", JsRaw("{\"key\" : \"value\"}"))}) )
      }

  }

  override def  receiveJson = { //: PartialFunction[JValue, JsCmd] = {
    case jvalue =>
      info("receiveJson(): jvalue: "+jvalue)
      this ! "update"
      net.liftweb.http.js.JsCmds.Noop
  }
}//class


