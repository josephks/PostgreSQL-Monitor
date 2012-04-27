package net.tupari.pgmon.comet
  import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._

import net.liftweb.widgets.flot._

/**
 * Created with IntelliJ IDEA.
 * User: jks
 * Date: 4/26/12
 * Time: 1:40 PM
 * To change this template use File | Settings | File Templates.
 */

case class JsFlotSetOps(idPlaceholder: String,
                        datas: List [FlotSerie],
                        newOptsList: List[List[ (String, JsExp)]]) extends JsCmd{
  def toJsCmd: String = {
    if (datas.size != newOptsList.size  ) Noop.toJsCmd
    else {
      val newValuePush: String = newOptsList.zipWithIndex.map
      {case ( newOpts, num) => {
        newOpts match{
          case null => ""
          case _ => //List[(String,JsExp)] =>
            val nameOptions = "datas_" + idPlaceholder + "[" + (num + 1) +"]"
            //multilayered keys a.b.c should not be treated as one key, obj['a.b.c'] rather as obj['a']['b']['c']
            //actually that doesn't work, needs to be turned into   a = {b : {c : val}}
            newOpts.map( {case (key, value) =>
              val list = key.split("\\.").toList
              nameOptions+"." + list.head + " = " + (   list.tail :\ (value.toJsCmd) ){
                (frag, acc) => "{" + frag+" : " + acc +"}"
              }       +" ;\n"
            }//case
            ).mkString("")
        }
      }}.reduceLeft (_ + _)

      val flotShow = Flot.renderFlotShow (idPlaceholder, datas, new FlotOptions{}, Noop).toJsCmd

      newValuePush + flotShow
    }
  }
}
