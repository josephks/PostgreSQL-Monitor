package net.tupari.pgmon.comet


import _root_.net.liftweb.util._
import net.liftweb.common.{Logger, Full}
import net.liftweb.http.js.JsCmds.{Replace, SetHtml}
import xml.{TopScope, NodeSeq, Text}
import net.liftweb.http.{SHtml, S, CometActor}
import  net.tupari.lib.SimpFactory

/** Bandwith data point */
class BwDataPoint(oa: List[Any], block_size: Int){
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
  def getBaseline = new BwDataPoint(List(stats_reset, 0, 0, 0, 0, stats_reset), block_size)
}

class PgBandwithActor  extends CometActor with Logger{

  val idBase = "pgbw"+SimpFactory.inject[ SimpFactory.UniqueNumber].get

  private var block_size = 8 * 1024 //8k is the default value

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

  override def localSetup() = {
    //todo: share this. There is no reason to do this every time the status page is loaded
    val showBlkszSql = "show block_size;"

    Common.getData(showBlkszSql) match{
      case Right( (keys, List(List(bs))) )  =>
        info("bs is a: "+bs.getClass)
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
    ".cssrtest" #> <span>cssrtest</span>
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
      case Some(errstr) =>
        <div class="error">{errstr}</div>
    }
 }

  Schedule.schedule(this, "update", 1L)

  override def lowPriority : PartialFunction[Any, Unit] = {
    case "update" =>
      //partialUpdate(Replace(rowId, getTableRow))
      doUpdate
      Schedule.schedule(this, "update", 2500L)
  }
}
