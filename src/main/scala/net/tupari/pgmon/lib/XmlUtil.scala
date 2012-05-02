package net.tupari.pgmon.lib

/**
 * Created with IntelliJ IDEA.
 * User: jks
 * Date: 5/2/12
 * Time: 2:29 PM
 * To change this template use File | Settings | File Templates.
 */

object XmlUtil {
  def joinNodeSeqs( seq: Seq[scala.xml.NodeSeq]) : scala.xml.NodeSeq = {
    val nb = new scala.xml.NodeBuffer
    seq.foreach( nb ++= _)
    nb
  }
  def toElem(nodeseq: scala.xml.NodeSeq)= {
    if (nodeseq.isInstanceOf[scala.xml.Elem]){
      nodeseq.asInstanceOf[scala.xml.Elem]
    }else if (nodeseq.size == 1 && nodeseq(0).isInstanceOf[scala.xml.Elem] ){
      nodeseq(0).asInstanceOf[scala.xml.Elem]
    }else{
      <span>{ nodeseq }</span>
    }
  }
}
