package net.tupari.pgmon.snippet

import xml.{Text, NodeSeq}
import net.liftweb.common.{Logger, Full}
import net.liftweb.util.Helpers
import net.tupari.pgmon.comet.{ TurnRefreshOn}

import net.liftweb.http.{SHtml, S, DispatchSnippet}
import net.liftweb.http.js.JsCmds
import net.tupari.lib._



class PgStatusPage  extends DispatchSnippet with Logger {

 def dispatch : DispatchIt = {  
    // We have to use a partially-applied (trailing "_") version
    // of the functions that we dispatch to
    case "serverName" => serverName _
    case "locksTable" => locksTable _
    case "backendsTable" => backendsTable _
    case "refreshAllButton" => refreshAllButton _
    case "bandwith" => bandwith _
    //case _ => catchAllMethod _
  }

  private val allActorNames = scala.collection.mutable.ListBuffer[String]()
  private lazy val serialNum = SimpFactory.inject[ SimpFactory.UniqueNumber]


  def autoRefreshCheckBox (xhtml : NodeSeq) : NodeSeq = {
    xhtml match {
      case Text(text) =>
        val actorClassName = text.trim() match {
          case "pgbackend" => "PgMonCometBackendsActor"
          case _ =>
            error("autoRefreshCheckBox: I don't understand "+text)
        }
      SHtml.ajaxCheckbox (false, { (b: Boolean) =>
	for ( actorName <- allActorNames ){
          S.session flatMap ( sess => sess.findComet("PgMonCometBackendsActor", Some(actorName)))
          match{
            case Full(act)  =>
              info("Monitor: refreshAll sending UpdateInfo to actor named " + actorName)
            act ! TurnRefreshOn(b)
            case _ =>
              val errMsg = "refreshAll: Could not find actor named " + actorName
            S.error(errMsg)
            error(errMsg)

          }}//for
                                })
      case x =>
        error("autoRefreshCheckBox: xhtml is: "+xhtml)
      <p>ERROR: xhtml is: { xhtml }</p>
    }
  }

  def backendsTable (xhtml : NodeSeq) : NodeSeq = {
    val actorName = "pgbackend" + serialNum 
    allActorNames += actorName
    <lift:comet type="PgMonCometBackendsActor" name={actorName} />
  }

  def locksTable (xhtml : NodeSeq) : NodeSeq = {
    <span id="lockstable" />
  }

  def bandwith  (xhtml : NodeSeq) : NodeSeq = {
        <p> placeholder, bandwith goes here </p>
  }
  def serverName  (xhtml : NodeSeq) : NodeSeq = {
        <p> placeholder, server name goes here </p>
  }
 def refreshAllButton  (xhtml : NodeSeq) : NodeSeq = {
        <p> placeholder, refresh button goes here </p>
  }
}
