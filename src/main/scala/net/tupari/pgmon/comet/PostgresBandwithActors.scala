package net.tupari.pgmon.comet


import _root_.net.liftweb.util._
import _root_.net.liftweb.util.TimeHelpers._
import net.liftweb.common.{Logger, Full}
import net.liftweb.http.{SHtml, S, CometActor}
import xml.{TopScope, NodeSeq, Text}
import net.tupari.lib.SimpFactory
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds._

import net.liftweb.widgets.flot._

/** Bandwith data point */
class BwDataPoint(oa: List[Any], block_size: Int, val baseline: Boolean = false){
  val timestamp = oa(0).asInstanceOf[java.sql.Timestamp]
  val buffers_alloc =  oa(1).asInstanceOf[java.lang.Number].longValue
  val buffers_checkpoint = oa(2).asInstanceOf[java.lang.Number].longValue
  val buffers_clean = oa(3).asInstanceOf[java.lang.Number].longValue
  val buffers_backend = oa(4).asInstanceOf[java.lang.Number].longValue
  val stats_reset = oa(5).asInstanceOf[java.sql.Timestamp]

  private def getBytesPerSec(bytes:Long,  last: BwDataPoint) =   block_size * 1000 * bytes / (timestamp.getTime - last.timestamp.getTime)
  def getBytesWritten =  buffers_checkpoint + buffers_clean + buffers_backend
  def getBpsWrittenSince(last: BwDataPoint) = getBytesPerSec (getBytesWritten - last.getBytesWritten, last)
  def getBpsReadSince(last: BwDataPoint) = getBytesPerSec(buffers_alloc - last.buffers_alloc, last)
  def getChptWSince(last: BwDataPoint) = getBytesPerSec(buffers_checkpoint - last.buffers_checkpoint, last)
  def getCleanWSince(last: BwDataPoint) = getBytesPerSec(buffers_clean - last.buffers_clean, last)
  def getBkndWSince(last: BwDataPoint) = getBytesPerSec(buffers_backend - last.buffers_backend, last)
  /** Get a dummy datapoint the represents the beginning of stats collection */
  def getBaseline = new BwDataPoint(List(stats_reset, 0, 0, 0, 0, stats_reset, true), block_size)
}

private object FlotTimeseriesOption extends  FlotAxisOptions{
  override def mode = Full("time")
}


class PgBandwithActor  extends CometActor with Logger{

  val idBase = "pgbw"+SimpFactory.inject[ SimpFactory.UniqueNumber].get

  private var block_size = 8 * 1024 //8k is the default value
   //see: http://www.postgresql.org/docs/9.1/static/monitoring-stats.html
  private val sql = "select now(),  buffers_alloc , buffers_checkpoint , buffers_clean , buffers_backend, stats_reset from pg_stat_bgwriter";

  private var prevDataPoint:BwDataPoint = null
  private  var lastDataPoint:BwDataPoint = null

  private var spanList:List[Dataspan] = Nil
  


  private sealed abstract class Dataspan(myIdBase: String){
    //seems hackish to me, to save a reference to myself in the constructor
    spanList = this :: spanList

    def getId = idBase + myIdBase
    def getSpan = <span id={ getId } >...</span>
  }
  private case class ReadBps() extends Dataspan("readbps"+SimpFactory.inject[ SimpFactory.UniqueNumber].get)
  private case class WriteBps() extends Dataspan("writebps"+SimpFactory.inject[ SimpFactory.UniqueNumber].get)
  private case class CheckpointWriteBps() extends Dataspan("cpwbps"+SimpFactory.inject[ SimpFactory.UniqueNumber].get)
  private case class CleaningWriteBps() extends Dataspan("cleanbps"+SimpFactory.inject[ SimpFactory.UniqueNumber].get)
  private case class BackendWriteBps() extends Dataspan("bkndbps"+SimpFactory.inject[ SimpFactory.UniqueNumber].get)
  private case class Timenow() extends Dataspan("timenow"+SimpFactory.inject[ SimpFactory.UniqueNumber].get)

  //represents a flot chart. In the future I will add options to render different kinds of charts,
  //so you could split data over different charts, or have charts with different time periods
  private case class FlotChart(origNode: scala.xml.NodeSeq){
    /** Number of data points on the x axis.  Updates will drop old data points off the left of the chart
     * to remain under this limit */
    private val timepoints = (origNode \ "@intervals").text match { case "" => 10 case x => x.toInt}
    /** dom id needed for javascript updates */
    private val domId = (origNode \ "@id").text
    private var dataLines = List[(String, LineOfData)]()

    /** Holds data for rendering a line on the chart
     * @param getNextDatapoint A function for getting the next datapoint to pass to flot */
    private case class LineOfData (  val getNextDatapoint: () => (Double,Double)){
      private[this] val data_values = List(getNextDatapoint())
      val data_to_plot:FlotSerie = new FlotSerie() {
        override val data = data_values
      }
    }

    private def init(){ //create the flot chart
      dataLines = List (
        ("readbps" -> new LineOfData( getNextDatapoint = () => (lastDataPoint.timestamp.getTime, lastDataPoint.getBpsReadSince(prevDataPoint)) )) ,
        ("writebps" -> new LineOfData( getNextDatapoint = () => (lastDataPoint.timestamp.getTime,lastDataPoint.getBpsWrittenSince(prevDataPoint)) )) ,
        ("checkpointwritebps" -> new LineOfData( getNextDatapoint = () =>(lastDataPoint.timestamp.getTime, lastDataPoint.getChptWSince(prevDataPoint)) ))  ,
        ("cleanwritebps" -> new LineOfData( getNextDatapoint = () => (lastDataPoint.timestamp.getTime,lastDataPoint.getCleanWSince(prevDataPoint)) )) ,
        ("backendwritebps" -> new LineOfData( getNextDatapoint = () => (lastDataPoint.timestamp.getTime,lastDataPoint.getBkndWSince(prevDataPoint))  ))
      )


      var flot_widget_rendered = Flot.render(domId, dataLines.map {
        case (str, lineofdata) => lineofdata.data_to_plot
      }, new FlotOptions {
        override def xaxis = Full(FlotTimeseriesOption)
      }, Flot.script(defaultHtml))

      import net.liftweb.http.js.JsCmds._
      partialUpdate(SetHtml(domId, Common.addResizePluginToFlotWidget(flot_widget_rendered)  ))
    }
    private var pointsDone = 0
    /** Called when a new datapoint comes in.  This method updates the flot chart */
    def onDataUpdate() = {
      if ( pointsDone == 0 || prevDataPoint.baseline)  {
        init()
        pointsDone = 1
      } else {
        val doPop = pointsDone >= timepoints  //If we have done timepoints worth of data already drop old datapoints
        pointsDone += 1
        partialUpdate( JsFlotAppendData( domId,
          dataLines.map{ case (_ , lod) => lod.data_to_plot},
          dataLines.map{ case (_, lod) => lod.getNextDatapoint() },
          doPop)   )
      }
    }

  } //class FlotChart

  private var flotCharts:List[FlotChart] = Nil

  override def localSetup() = {
    //todo: share this. There is no reason to do this every time the status page is loaded
    val showBlkszSql = "show block_size;"

    Common.getData(showBlkszSql) match{
      case Right( (keys, List(List(bs))) ) =>
        block_size = bs.asInstanceOf[String].toInt
        case Left(errstr) =>
        error(errstr)
      case x  =>
        error("code bug: block size query returned: " +x)
    }
  }

  def render = {
    ".totalreadbps" #> new ReadBps().getSpan &  
    ".totalwritebps" #> new WriteBps().getSpan &  
    ".checkpointwritebps" #> new CheckpointWriteBps().getSpan &  
    ".cleanwritebps" #> new CleaningWriteBps().getSpan &  
    ".backendwritebps" #>   new BackendWriteBps().getSpan &  
    ".timenow" #> new Timenow().getSpan &
    ".cssrtest" #> <span>cssrtest</span> &
    ".bwgraph"  #> { (node: scala.xml.NodeSeq) => {    //css selector of type Node => Node
      (node \ "@id").text match{
        case "" =>
          //if node does not have an id add one
          val newNode = node.asInstanceOf[scala.xml.Elem] % new scala.xml.UnprefixedAttribute("id", "graph"+SimpFactory.inject[ SimpFactory.UniqueNumber].get, scala.xml.Null)
          flotCharts = new FlotChart(newNode) :: flotCharts
          newNode
        case _ =>
           flotCharts = new FlotChart(node) :: flotCharts
          node
      } //match
    }   }
  }

  /** Return an error string, if any */
  private def setDataPoint: Option[String] = {
    Common.getData(sql) match{
      case Right( (keys, oaa) ) =>
        prevDataPoint = lastDataPoint
        lastDataPoint = new BwDataPoint(oaa(0), block_size)
        //if this is the first data fetch, or there was a stats reset, set the prev data point to be a baseline
        if (prevDataPoint == null || prevDataPoint.stats_reset != lastDataPoint.stats_reset)
          prevDataPoint = lastDataPoint.getBaseline
        None
      case Left(errstr) =>
        Some(errstr)
    }
  }
  private val numFormat = java.text.NumberFormat.getInstance

  def doUpdate = {
    setDataPoint match{
      case None =>
        //info("doUpdate running")
        for(span <- spanList){
          val text =  span match{
            case ReadBps() =>
              numFormat.format(lastDataPoint.getBpsReadSince(prevDataPoint))
            case WriteBps() =>
              numFormat.format(lastDataPoint.getBpsWrittenSince(prevDataPoint))
            case CheckpointWriteBps() =>
              numFormat.format(lastDataPoint.getChptWSince(prevDataPoint))
            case CleaningWriteBps()  =>
              numFormat.format(lastDataPoint.getCleanWSince(prevDataPoint) )
            case BackendWriteBps() =>
              numFormat.format(lastDataPoint.getBkndWSince(prevDataPoint))
            case Timenow() =>
              lastDataPoint.timestamp.toString.split('.')(0)
          }
          partialUpdate(SetHtml(span.getId, <div>{ text }</div>))
          //info("did partial update for span "+span.getId)
        } //for span
        for(flotChart <- flotCharts){
          flotChart.onDataUpdate()
        }
      case Some(errstr) =>
        <div class="error">{errstr}</div>
    }
 }

  Schedule.schedule(this, "update", 1L)

  override def lowPriority : PartialFunction[Any, Unit] = {
    case "update" =>
      doUpdate

      if (false){
        //old: server side scheduling of next push:
        Schedule.schedule(this, "update", 2500L)
        //I just keep this around in case I need it for debugging purposes
      }else{
        //new: client side request for update, to avoid pushing over a bad connection
        val json_send_jscmd = //jsonSend(net.liftweb.http.js.JE.Num(666))
          jsonSend("update")
        debug("json send cmd: "+json_send_jscmd)
        partialUpdate( After(2000 millis, json_send_jscmd   ))
        //partialUpdate( After(2500 millis,{jsonSend( net.liftweb.http.js.JE.Num(666)) }) )
        debug("sent after command")
      }
  }

    override def  receiveJson = { //: PartialFunction[JValue, JsCmd] = {
    case jvalue =>
             debug("receiveJson(): jvalue: "+jvalue)
            this ! "update"
            net.liftweb.http.js.JsCmds.Noop
//2012-02-09 23:42:02,041 [pool-186-thread-8] INFO  n.t.p.c.PgBandwithActor - receiveJson(): jvalue: JObject(List(JField(command,JInt(666)), JField(params,JBool(false))))
//2012-02-09 23:42:02,056 [pool-186-thread-8] INFO  n.t.p.c.PgBandwithActor - json send cmd: JsCmd(F1067007676534G452R4({'command': 666, 'params': false});)
//  receiveJson(): jvalue: JObject(List(JField(command,JString(update)), JField(params,JBool(false))))

  }
     override def  autoIncludeJsonCode = true
}
