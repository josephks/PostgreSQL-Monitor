package net.tupari.pgmon.comet

import _root_.net.liftweb.util._
import _root_.net.liftweb.util.TimeHelpers._
import net.liftweb.common.Logger
import net.liftweb.http.CometActor
import net.tupari.lib.SimpFactory

import net.liftweb.http.js.JsCmds._

import net.tupari.pgmon.lib.Implicits._
import net.liftweb.db.{DefaultConnectionIdentifier, ConnectionIdentifier}
import net.tupari.pgmon.lib.{TableCreator, ConnectionData}


/**
 * Comet actor for monitoring the health of a Postgres primary to secondary sync.
 * NOTE: right now only works for installations with one secondary.
 * 
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 3/13/12
 * Time: 11:47 AM
 */

class PgSecondaryActor extends CometActor with net.liftweb.common.LazyLoggable{
  // http://www.postgresql.org/docs/9.1/interactive/monitoring-stats.html#MONITORING-STATS-VIEWS-TABLE
  private val primary_sql = "select *, version() as primary_version from pg_stat_replication;"
  private val secondary_sql = "select pg_is_in_recovery(), pg_last_xlog_receive_location()," +
    " pg_last_xlog_replay_location(),pg_last_xact_replay_timestamp(), version() as secondary_version ;"

  lazy val primary: ConnectionIdentifier = (defaultHtml \ "@primary").text.trim match{
    case "" => DefaultConnectionIdentifier
    case s => s
  }
  override def localSetup() = {  }

  /** Future that holds info from the primary server */
  private var primaryFut: scala.actors.Future[Either[String, Tuple2[ List[String],List[List[Any]] ] ] ] = null

  class SecondaryMonitor(originalNode: scala.xml.NodeSeq){
    val secondary:String = (originalNode \ "@secondary").text.trim
    lazy val hostAndPort = ConnectionData.ipmap(secondary) //tuple

    private val secondaryDbConn: ConnectionIdentifier = secondary  //implicit conversion from String


    private case class UpdatableSpan(id: String = "span"+SimpFactory.inject[ SimpFactory.UniqueNumber].get, fieldname: String ) {
      def getSpan = <span id={ id } >...</span>
      def update(map: Map[String, Any]) = {
        //todo: better error handling

        //map.get() can return a Some(null), so defend against that
        val text = map.get(fieldname).getOrElse("") match { case null => "" ; case x => x.toString }
        partialUpdate(SetHtml(id, scala.xml.Text( text ) ))
      }
    }
    /** A span that generates a table from all the available fields, for those too lazy to create a custom template.
     * The fields end up in random order, so this is not recommended. */
    private class WholeTableSpan(id: String = "span"+SimpFactory.inject[ SimpFactory.UniqueNumber].get) extends UpdatableSpan(id, ""){
      override def getSpan = <span id={ id } > table goes here</span>
      override  def update(map: Map[String, Any]) = {
        partialUpdate(SetHtml(id,TableCreator(map).getTable(None) ) )
      }
    }
    private var updatableSpans : List[UpdatableSpan] = Nil

    lazy val realSpan = {
      logger.debug("realspan called, orig is : "+originalNode)
      import xml.transform.{RewriteRule, RuleTransformer}
      object rwr  extends RewriteRule {
        override def transform(n: scala.xml.Node): Seq[scala.xml.Node] ={
          n match{
            //doesn't work: case <div  >{ _ }</div> if (n \ "@class").text != "" =>
             case _ if n != originalNode && (n.label == "div" || n.label == "span") && (n \ "@class").text != "" =>
              (n \ "@class").text  match{
                //case "chart" => n  //don't replace the top level
                case "wholetable" =>
                  val u = new WholeTableSpan
                   updatableSpans = u :: updatableSpans
                  u.getSpan
                case "primarysql" =>  <span class="sql">{ primary_sql} </span>
                case "secondarysql" =>  <span class="sql">{ secondary_sql} </span>
                //If not one of the above options, assume the classname is the name of a field from the sql query
                case classname if ( (originalNode \ "@class").text != classname)  =>
                  val u =  UpdatableSpan(fieldname = classname)
                  updatableSpans = u :: updatableSpans
                  u.getSpan
                case _ =>
                  n
              }
            case _ =>
              n
          }
        }
      }
      new RuleTransformer(rwr).transform(originalNode)
    } //lazy val

    val id = "secondary"+SimpFactory.inject[ SimpFactory.UniqueNumber].get
    def getSpan = if (secondary == "") { <span class="error">Secondary is not set</span> } else {
      logger.debug("getSpan returning " + realSpan)
      realSpan
    }
    def doUpdate() {
      var map = Map[String,Any]()
      Common.getDataFromConnection(secondary_sql, target_host = secondaryDbConn)
      match{
        case Right( (keys, oaa) ) =>
          map = keys.zip(oaa(0)).toMap

        case Left(errstr) =>
          logger.error(errstr)
      }
      try{
        //BUG: don't use foreach: primaryFut.foreach({
        //see  https://issues.scala-lang.org/browse/SI-5574
        primaryFut.apply() match{
        case Right( (keys, oaa) ) =>
          map ++=   keys.zip(oaa(0)).toMap //todo: fix, find data row for this secondary based on ip address and port.  This just works for installations with one secondary

        case Left(errstr) =>
          logger.error(errstr)
      }
        }catch{
        case e => logger.warn("primaryFut.foreach caused ex",e)
      }
      updatableSpans.foreach( _.update(map))
    }
  } //SecondaryMonitor

  private var secondaryMonitorList:List[SecondaryMonitor] = Nil

  private def render0 = {
    logger.debug("render() called primary="+primary)
    ".reptable" #>  { (node: scala.xml.NodeSeq) => {    //css selector of type Node => Node
      val ans = new SecondaryMonitor( node )
      secondaryMonitorList = ans :: secondaryMonitorList
      ans.getSpan
    }   }
  }
  private lazy val renderOnce = {
    val ans = render0
    logger.debug("renderOnce returing "+ans)
    Schedule.schedule(this, "update", 1000L)
    ans
  }
  def render = renderOnce

  private def doUpdate() {
    logger.debug("doUpdate starting")
    //first ste primaryFut.  All secondaries will share this information
    try{
      primaryFut = scala.actors.Futures.future{ Common.getDataFromConnection(primary_sql, target_host = primary) }
    }catch{
      case e => logger.warn("setting primaryFut caused ex",e)
    }
    secondaryMonitorList.foreach({ x =>
      try{
        x.doUpdate()
      }catch{
        case e => logger.warn("secondaryMonitor.doUpdate caused "+e.getClass.getName+" : "+e.getMessage)
      }
    } )
    logger.debug("doUpdate finished")
  }

  override def lowPriority : PartialFunction[Any, Unit] = {
    case "update" =>
      doUpdate()

      if (false){
        //old: server side scheduling of next push:
        Schedule.schedule(this, "update", 2500L)
        //I just keep this around in case I need it for debugging purposes
      }else{
        //new: client side request for update, to avoid pushing over a bad connection
        val json_send_jscmd = jsonSend("update")
        logger.debug("json send cmd: "+json_send_jscmd)
        partialUpdate( After(2000 millis, json_send_jscmd   ))
        logger.debug("sent after command")
      }
  }

  override def  receiveJson = { //: PartialFunction[JValue, JsCmd] = {
    case jvalue =>
      logger.debug("receiveJson(): jvalue: "+jvalue)
      this ! "update"
      net.liftweb.http.js.JsCmds.Noop
  }
  override def  autoIncludeJsonCode = true

}
        /*
          procpid | usesysid |  usename   | application_name | client_addr  | client_hostname | client_port |        backend_start         |   state   | sent_location | write_location | flush_location | replay_location | sync_priority | sync_sta
te
---------+----------+------------+------------------+--------------+-----------------+-------------+------------------------------+-----------+---------------+----------------+----------------+-----------------+---------------+---------
---
   25264 |  2364898 | replicator | walreceiver      | 10.10.10.104 |                 |       37525 | 2012-03-02 13:54:39.93637-05 | streaming | 18/27000000   | 18/27000000    | 18/27000000    | 18/27000000     |             0 | async
(1 row)
    postgres-# pg_last_xact_replay_timestamp() ;
 pg_is_in_recovery | pg_last_xlog_receive_location | pg_last_xlog_replay_location | pg_last_xact_replay_timestamp
-------------------+-------------------------------+------------------------------+-------------------------------
 t                 | 18/29000000                   | 18/29000000                  | 2012-03-13 12:49:33.13222-04
(1 row)

Time: 1.352 ms

            One row per WAL sender process, showing process ID, user OID, user name, application name, client's address, host name (if available) and port number, time at which the server process began execution, and the current WAL sender state and transaction log location. In addition, the standby reports the last transaction log position it received and wrote, the last position it flushed to disk, and the last position it replayed, and this information is also displayed here. If the standby's application names matches one of the settings in synchronous_standby_names then the sync_priority is shown here also, that is the order in which standbys will become the synchronous standby. The columns detailing what exactly the connection is doing are only visible if the user examining the view is a superuser. The client's host name will be available only if log_hostname is set or if the user's host name needed to be looked up during pg_hba.conf processing.

  jks=# select pg_is_in_recovery(), pg_last_xlog_receive_location(), pg_last_xlog_replay_location(),pg_last_xact_replay_timestamp();
 pg_is_in_recovery | pg_last_xlog_receive_location | pg_last_xlog_replay_location | pg_last_xact_replay_timestamp
-------------------+-------------------------------+------------------------------+-------------------------------
 t                 | 0/40000078                    | 0/40000078                   |
(1 row)


*/
