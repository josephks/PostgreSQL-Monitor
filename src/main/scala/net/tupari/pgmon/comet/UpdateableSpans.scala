package net.tupari.pgmon.comet

import net.liftweb.http.CometActor
import net.liftweb.http.js.JsCmds.SetHtml

/**
 * Created with IntelliJ IDEA.
 * User: jks
 * Date: 5/2/12
 * Time: 3:28 PM
 * To change this template use File | Settings | File Templates.
 */

trait UpdateableSpans {

      this: CometActor =>

  class UpdateableSpan(val uuid: String){
    def getSpan(node: scala.xml.Node): scala.xml.Elem = <span id={ uuid } >{ node }</span>
    def getSpan: scala.xml.Elem = getSpan(scala.xml.Text("..."))//return a placeholder span to be inserted into html on doc creation
    def setHtml(node: scala.xml.Node)  = partialUpdate(SetHtml(uuid, node))
    def setHtml(text: String)  = partialUpdate(SetHtml(uuid, scala.xml.Text(text)))
  }
}
