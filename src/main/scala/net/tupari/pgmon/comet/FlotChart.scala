package net.tupari.pgmon.comet

import scala.xml.Text

import net.liftweb.widgets.flot._

import _root_.net.liftweb.util._
import net.liftweb.http.{SHtml, S, CometActor}
import net.liftweb.common.{Box, Logger, Full}

import xml.{TopScope, NodeSeq, Text}
import net.tupari.lib.SimpFactory
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds._

import net.tupari.lib.{SimpFactory }
        import   net.tupari.pgmon.lib.XmlUtil
    import net.tupari.pgmon.lib.Implicits._

/**
 * Mix in to comet actors to use. Encapsulates logic for a simple Flot chart (wraps the Lift Flot widget).
 *
 * Created with IntelliJ IDEA.
 * User: jks
 * Date: 5/2/12
 * Time: 3:11 PM
 */
trait FlotCharts {

  this: CometActor with UpdateableSpans =>

  class FlotChart(origHtml: scala.xml.Elem, dataLabels: List[String], lineColors: List[String]){
    private val DEFAULT_INTERVALS = 100

    class MyFlotSerie(val lbl: String, val _color: String) extends  net.liftweb.widgets.flot.FlotSerie{
      override def label: Box[String] = Full(lbl)
      override def color: Box[Either[String, Int]] = Full(Left(_color) )
    }

    var checkBoxSpanId: Option[String] = None
    /** Element id of the html element being used for the flot chart */
    val uuid = "flotspan"+SimpFactory.inject[ SimpFactory.UniqueNumber].get


    //html2 is original with checkboxes div replaced
    private val html2 = XmlUtil.toElem( (".checkboxes" #> ((checkboxDiv: scala.xml.NodeSeq) ⇒ {
      val ans = new UpdateableSpan("cbx"+SimpFactory.inject[ SimpFactory.UniqueNumber].get)
      checkBoxSpanId = Some(ans.uuid)
      ans.getSpan(Text(""))
    })).apply(origHtml) )
    //html3 is html2, with the class attribute of the root possibly set to "flotchart" (if no child was so designated we use the top level as one)
    //flot_span is the Elem that houses the actual chart
    //timepoints is the number of datapoints we keep on the chart before scrolling old ones off as new ones replace them
    private val (html3, flot_span, timepoints) = html2.searchForNodeWithAttrib( "class" -> "flotchart") match {
      case Some(chartnode) => (html2, //unchanged
        chartnode,
        (chartnode \ "@intervals").text match { case "" => DEFAULT_INTERVALS case x => x.toInt} )
      case None =>
        //If no child node is annotated with  class="flotchart" set it on the root
        val newroot = html2 % new scala.xml.UnprefixedAttribute("class", "flotchart", scala.xml.Null) //use top level
        (newroot, newroot, DEFAULT_INTERVALS)
    }
    //now use a css transform on html3 to create the final span
    val getSpan = (".flotchart" #> ((chartDiv: scala.xml.NodeSeq) ⇒ { chartDiv.asInstanceOf[scala.xml.Elem] % new scala.xml.UnprefixedAttribute("id", uuid, scala.xml.Null) })).apply(html3)


    var data_lines: List[FlotSerie] = null //set in handleFirstDataPoint()
    var pointsDone = 0
    var show_toggle: Array[Boolean] = null  //corresponds to each FlotSerie


    private def handleFirstDataPoint(newdata: List[(Double, Double)]) = { //on first datapoint render the chart
      data_lines = dataLabels.zip(lineColors).zip(newdata).map( {case ((op, color), datum) =>
        new MyFlotSerie(op, color){
          override val data = List( datum )
        }
      })
      show_toggle = dataLabels.map( _ =>  true).toArray

      //get series to be rendered, based on if their boolean is set in show_toggle
      def serieToRender = show_toggle.toList.zip(data_lines).filter{ case (b, _) => b }.map{ case (_, s) => s  }

      class MyFlotOptions extends FlotOptions {
        override def xaxis = Full(
          new  FlotAxisOptions{
            override def mode = Full("time")
          })
      }

      val flot_widget_rendered =  Flot.render(uuid, data_lines, new MyFlotOptions, Flot.script(flot_span))

      partialUpdate(SetHtml(uuid, flot_widget_rendered ))
      //now create the checkboxes.  Can't be created until data_lines is created

      val USE_CLIENT_SIDE = true //Use client side javascript alternative.
      //I'm keeping the server side code around in case some future change to the flot widget breaks the client side code here

      lazy val legend_checkbox_server_side =  <label> {SHtml.ajaxCheckbox (true, { (b: Boolean) =>
        val newOptions = new MyFlotOptions{ override def legend = Full(new FlotLegendOptions{ override def show = Full(b)})}
        //Doesn't work on its own. The options passed into renderFlotShow() are not actually used in its code
        //                         Flot.renderFlotShow ( uuid,   serieToRender, newOptions, Noop)
        //Instead set the variable, then call  Flot.renderFlotShow() to redraw
        net.liftweb.http.js.JsCmds.JsCrVar("options_"+uuid, newOptions.asJsObj) &
          Flot.renderFlotShow ( uuid, null, null, Noop)
      } ) }Legend </label>

      if (USE_CLIENT_SIDE){ //If using client side alternatives declare functions to be used in the onclick="" of the checkboxes
        val options_var_name = "options_"+uuid
        val datas_var_name = "datas_"+uuid
        val show_toggle =    "show_toggle_"+uuid

        partialUpdate(net.liftweb.http.js.JE.JsRaw(
          """function onLgndClick_""" +   uuid + """(b){
          """+options_var_name+""" = jQuery.extend( """+options_var_name+"""  , { legend: { show: b}}   ) ;
                             """ + Flot.renderFlotShow ( uuid, null, null, Noop).toJsCmd + """
                 }

                 var """ +  show_toggle  + """ = [ """ +  (for(_ <- 1 to data_lines.length) yield true).mkString(",")+ """]  ;
                 var orig_""" +   datas_var_name + """ =  jQuery.extend( {}, """ +   datas_var_name + """ ) ;

                 function onMemOpClick_""" +   uuid + """(b, idx){
                    """ +  show_toggle  + """[idx] = b  ;
                     """ +   datas_var_name + """.splice(0) ; //clear array. Works even though isn't really an array. Setting = {} probably works just as well
                     for(var i = 0; i <  """ +  show_toggle + """.length ; i++){
                        if (""" +   show_toggle + """[i])
                             """ +   datas_var_name + """.push( orig_""" +   datas_var_name + """[i] ) ;
                     }
                     """ + Flot.renderFlotShow ( uuid, null, null, Noop).toJsCmd + """
                 }
                 """
        ).cmd)
      }

      lazy val legend_checkbox_client_side =   <label> <input checked="checked" type="checkbox" onclick={ "onLgndClick_" +  uuid + "(this.checked)"} /> Legend </label>

      checkBoxSpanId match{
        case None =>
        case Some(cbx_id) =>
          partialUpdate(SetHtml(cbx_id, XmlUtil.joinNodeSeqs(
            //join a list of one item (legend toggle checkbox) with a list of checkboxes created from dataLabels
            List(if (USE_CLIENT_SIDE){ legend_checkbox_client_side }else{ legend_checkbox_server_side }) ++
              dataLabels.zipWithIndex.map{case (name, idx) =>
                if (USE_CLIENT_SIDE){
                  <label> <input checked="checked" type="checkbox" onclick={"onMemOpClick_" +  uuid + "(this.checked, " + idx+")"} />
                    { name } </label>
                } else { //use original server side method
                  <label> {
                    SHtml.ajaxCheckbox (true, { (b: Boolean) =>
                    //This makes the line invisible, but still on the plot
                    //                    JsFlotSetOps(uuid, data_lines,
                    //                             {  val array = Array.fill[List[ (String, JsExp)]](memops.size)(Nil)
                    //                              array(idx) = List(("lines.lineWidth", if(b){ 2 }else{ 0 }))
                    //                              array.toList
                    //                            })
                      show_toggle(idx) = b
                      //now rerender widget
                      net.liftweb.http.js.JsCmds.JsCrVar("datas_"+uuid, Flot.renderSeries(serieToRender, uuid)) &
                        Flot.renderFlotShow (uuid, null, null, Noop)
                    } ) }{ name} </label>  } }) ))
      }//match
    } //handleFirstDataPoint()

    def doUpdate(newdata: List[(Double, Double)]) = {
      if (pointsDone == 0){
        handleFirstDataPoint(newdata)
      }else{
        val doPop = pointsDone >= timepoints  //If we have done timepoints worth of data already drop old datapoints
        //Update flot chart here
        partialUpdate( JsFlotAppendData( uuid, data_lines,  newdata,  doPop)  )
      }
      pointsDone += 1
    } //updateData
  }
}
