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
import net.tupari.pgmon.lib.TableCreator

/**
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 1/10/12
 * Time: 3:55 PM
 * To change this template use File | Settings | File Templates.
 */

  //for future use, to get the stats for an individual table given its oid
//select (select attname from pg_catalog.pg_attribute where staattnum = attnum AND attrelid = starelid),* from pg_statistic where starelid = 27192;

class PgInstallationSizeActor  extends CometActor with Logger{

  private val installationSizeSql =  "SELECT d.datname, pg_database_size(d.datname), age(d.datfrozenxid) FROM pg_catalog.pg_database d;"
  lazy val tableId = "inssz"   + SimpFactory.inject[ SimpFactory.UniqueNumber].getOrElse("")
  lazy val dateSpanId = "dspan" + SimpFactory.inject[ SimpFactory.UniqueNumber].getOrElse("")
  var refreshOn = false
   private var hasDate = false

  override protected def dontCacheRendering: Boolean = true


  def render = {

    ".installsizetable" #> <table  > <tbody id={ tableId }>{ getTableContents } </tbody></table> &
      ".installsizesql" #> <span>{ installationSizeSql } </span> &
       ".freshnessdate" #> { hasDate = true ;  <span id={ dateSpanId } /> } &
      ".reloadbox" #>   SHtml.ajaxCheckbox (false, { (b: Boolean) =>
        refreshOn = b
        if (b) this ! "update"
      })
  }

  private def getTableContents: NodeSeq = {
    debug("getTableContents starting")
    val data = Common.getData(installationSizeSql)
    data match{
      case Right( (keys, oaa) ) =>
        new TableCreator(keys, oaa).getTableContents
      case Left(errstr) =>
        <tr><td class="error">errstr</td></tr>
      case _ =>
        <tr><td class="error">code error in { this.getClass }</td></tr>
    }
  }

  override def lowPriority : PartialFunction[Any, Unit] = {
    case "update" =>
      partialUpdate(SetHtml(tableId, getTableContents))
      if (hasDate)
          partialUpdate(SetHtml(dateSpanId, Text(now.toString) ))
      if (refreshOn  )
        Schedule.schedule(this, "update", 2500L)

  }
}

class PgTableStatsActor  extends CometActor with Logger{

  val statsSql = "select * from pg_stat_all_tables;"
  lazy val tableId = "tabstats"   + SimpFactory.inject[ SimpFactory.UniqueNumber].getOrElse("")
  lazy val dateSpanId = "dspan" + SimpFactory.inject[ SimpFactory.UniqueNumber].getOrElse("")
  var refreshOn = false
   private var hasDate = false

  override protected def dontCacheRendering: Boolean = true


  def render = {

    ".relstatstable" #> <table  > <tbody id={ tableId }>{ getTableContents } </tbody></table> &
      ".relstatssql" #> <span>{ statsSql } </span> &
       ".freshnessdate" #> { hasDate = true ;  <span id={ dateSpanId } /> } &
    ".reloadbutton" #>       SHtml.ajaxButton ("refresh", () => { this ! "update" ; net.liftweb.http.js.JsCmds._Noop}) &
      ".reloadbox" #>   SHtml.ajaxCheckbox (false, { (b: Boolean) =>
        refreshOn = b
        if (b) this ! "update"
      })
  }

  private def getTableContents: NodeSeq = {
    debug("getTableContents starting")
    val data = Common.getData(statsSql)
    data match{
      case Right( (keys, oaa) ) =>
        new TableCreator(keys, oaa).getTableContents
      case Left(errstr) =>
        <tr><td class="error">errstr</td></tr>
      case _ =>
        <tr><td class="error">code error in { this.getClass }</td></tr>
    }
  }

  override def lowPriority : PartialFunction[Any, Unit] = {
    case "update" =>
      partialUpdate(SetHtml(tableId, getTableContents))
      if (hasDate)
          partialUpdate(SetHtml(dateSpanId, Text(now.toString) ))
      if (refreshOn  )
        Schedule.schedule(this, "update", 2500L)

  }
}


class PgRelationSizeActor  extends CometActor with Logger{
  val relationSizeQuery =  "select (select nspname FROM pg_catalog.pg_namespace where oid = relnamespace) AS schema"+
        ", relname,  CASE c.relkind WHEN 'r'"+
        " THEN 'table' WHEN 'v' THEN 'view' WHEN 'i' THEN 'index' WHEN 'S' THEN 'sequence' WHEN"+
        " 's' THEN 'special' WHEN 'c' THEN 'composite type' WHEN 't' THEN 'toast' ELSE c.relkind::char END as \"Type\",  pg_relation_size(oid) AS bytes, CASE c.relkind IN('r','t') WHEN true THEN pg_total_relation_size(oid) END AS total_relation_size"+
        ", CASE relpages > 0 WHEN true THEN reltuples/relpages END AS tuplesperpage,"+
                   " oid::text, CASE c.relkind WHEN 'r' THEN age(relfrozenxid) END AS xid_age,c.reloptions,(SELECT GREATEST(last_vacuum, last_autovacuum) FROM pg_stat_all_tables WHERE relid = oid) AS last_vac"+
                   " FROM pg_catalog.pg_class c ORDER BY schema, relname";

     lazy val tableId = "tabstats"   + SimpFactory.inject[ SimpFactory.UniqueNumber].getOrElse("")
  lazy val dateSpanId = "dspan" + SimpFactory.inject[ SimpFactory.UniqueNumber].getOrElse("")
  var refreshOn = false
   private var hasDate = false

  override protected def dontCacheRendering: Boolean = true


  def render = {

    ".relsizetable" #> <table  > <tbody id={ tableId }>{ getTableContents } </tbody></table> &
      ".relsizesql" #> <span>{ relationSizeQuery } </span> &
       ".freshnessdate" #> { hasDate = true ;  <span id={ dateSpanId } /> } &
         ".reloadbutton" #>        SHtml.ajaxButton ("refresh", () => { this ! "update" ; net.liftweb.http.js.JsCmds._Noop}) &
      ".reloadbox" #>   SHtml.ajaxCheckbox (false, { (b: Boolean) =>
        refreshOn = b
        if (b) this ! "update"
      })
  }

  private def getTableContents: NodeSeq = {
    debug("getTableContents starting")
    val data = Common.getData(relationSizeQuery)
    data match{
      case Right( (keys, oaa) ) =>
        new TableCreator(keys, oaa).getTableContents
      case Left(errstr) =>
        <tr><td class="error">errstr</td></tr>
      case _ =>
        <tr><td class="error">code error in { this.getClass }</td></tr>
    }
  }
}