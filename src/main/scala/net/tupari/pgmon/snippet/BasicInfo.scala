package net.tupari.pgmon.snippet

import net.liftweb.http.{SHtml, S, DispatchSnippet}

import net.liftweb.common.{Logger, Full}
import net.tupari.pgmon.comet.Common
import xml.NodeSeq

/**
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 2/8/12
 * Time: 7:07 PM
 */

class BasicInfo  extends DispatchSnippet with Logger{
    def dispatch : DispatchIt = {
    // We have to use a partially-applied (trailing "_") version
    // of the functions that we dispatch to
    case "version" => version _
    case "dbencoding" => dbencoding _
    }

    lazy val serverInfo = getServerInfo

  private def getServerInfo = {
    Common.getData("SELECT  version(), getdatabaseencoding();")

  }

  def version (xhtml : NodeSeq): NodeSeq = {
    serverInfo     match{
      case Right( (keys, oaa) ) =>
        new scala.xml.Text(keys.zip(oaa(0)).toMap.apply("version").toString)
      case Left(errstr) =>
        <div class="error">{ errstr }</div>
      case _ =>
        <div class="error">code error in { this.getClass }</div>
    }

  }
  def dbencoding (xhtml : NodeSeq) : NodeSeq = {
      serverInfo     match{
      case Right( (keys, oaa) ) =>
        new scala.xml.Text(keys.zip(oaa(0)).toMap.apply("getdatabaseencoding").toString)
      case Left(errstr) =>
        <div class="error">{ errstr }</div>
      case _ =>
        <div class="error">code error in { this.getClass }</div>
    }

  }

}
