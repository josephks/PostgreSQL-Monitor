package net.tupari.pgmon.comet


import _root_.net.liftweb.util._
//import    _root_.net.liftweb.util.TimeHelpers._
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

  def getBytesWritten =  buffers_checkpoint + buffers_clean + buffers_backend
  def getBpsWrittenSince(last: BwDataPoint) = block_size * 1000 * (getBytesWritten - last.getBytesWritten) / (timestamp.getTime() - last.timestamp.getTime())
  def getBpsReadSince(last: BwDataPoint) = block_size * 1000 * (buffers_alloc - last.buffers_alloc) / (timestamp.getTime() - last.timestamp.getTime())
  def getChptWSince(last: BwDataPoint) = block_size * 1000 * (buffers_checkpoint - last.buffers_checkpoint) / (timestamp.getTime() - last.timestamp.getTime())
  def getCleanWSince(last: BwDataPoint) = block_size * 1000 * (buffers_clean - last.buffers_clean) / (timestamp.getTime() - last.timestamp.getTime())
  def getBkndWSince(last: BwDataPoint) = block_size * 1000 * (buffers_backend - last.buffers_backend) / (timestamp.getTime() - last.timestamp.getTime())
}

class PgBandwithActor  extends CometActor with Logger{

  val idBase =  "pgbw"+SimpFactory.inject[ SimpFactory.UniqueNumber].get
  val ids = List("totalwr", "totalr", "chkw", "cleanw", "backw")

   private val block_size = 8 * 1024 //todo: get from db, don't assume

   private val sql = "select now(),  buffers_alloc , buffers_checkpoint , buffers_clean , buffers_backend from pg_stat_bgwriter";
   private var tableId = "pgbwtbl"
   private var rowId = tableId + "row"

  private  val numCols = "2"
  
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


  def render = {
    ".totalreadbps" #> new ReadBps().getSpan &  
    ".totalwritebps" #> new WriteBps().getSpan &  
    ".checkpointwritebps" #> new CheckpointWriteBps().getSpan &  
    ".cleanwritebps" #> new CleaningWriteBps().getSpan &  
    ".backendwritebps" #>   new BackendWriteBps().getSpan &  
    ".timenow" #> new Timenow().getSpan &
    ".cssrtest" #> <span>cssrtest</span>
  }
  def placeholder = NodeSeq.Empty

  /** Return an error string, if any */
  private def setDataPoint: Option[String] = {
    Common.getData(sql) match{
      case Right( (keys, oaa) ) =>
	prevDataPoint = lastDataPoint
      lastDataPoint = new BwDataPoint(oaa(0), block_size)
      None
      case Left(errstr) =>
	       Some(errstr)
    }
  }
private val numFormat = java.text.NumberFormat.getInstance

 def doUpdate = {
   setDataPoint match{
     case None =>
       prevDataPoint match {
	 case null =>
	   //first time, do nothing
           info("first datapnt, doing nothing")
	 case _ =>
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
                 lastDataPoint.timestamp
             }
             partialUpdate(SetHtml(span.getId, <div>{ text }</div>))
             //info("did partial update for span "+span.getId)
           }
         //info("processed "+spanList.size+" entries")
       }
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
