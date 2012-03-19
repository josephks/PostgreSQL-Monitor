package net.tupari.pgmon.lib

import net.liftweb.common.{Logger, Full}
import xml.{NodeSeq, Node}

/**
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 1/7/12
 * Time: 9:11 PM
 */

object TableCreator{
  /** Create a TableCreator from a Map.  The fields end up in random order, so this is not recommended */
  def apply(map: Map[String, Any]) = {
    val (kl, vl) = ((List[String](), List[Any]()) /: map) {
     // case ((kl:List[String], vl:List[Any]) , (k, v)) => (k :: kl, v :: vl)
      case ((kl, vl) , (k, v)) => (k :: kl, v :: vl)
    }
    new TableCreator(kl, List(vl))
  }
   def apply( keys: List[String], data: List[List[Any]]) = new TableCreator(keys, data)
}

class TableCreator(keys: List[String], data: List[List[Any]]) extends Logger{
  //val nodeBuf = new scala.xml.NodeBuffer

  val keysToIgnore: List[String] = List()

  protected def getHeaderRow: Seq[Node] = {
    <tr> { keys.filterNot(key => keysToIgnore.contains(key)).map( key => getHeaderNodes( key ) ) } </tr>
  }
  protected def getHeaderNodes(key: String ): Seq[Node] = {
    <th> { key } </th>
  }
  var rowodd = true
  /** meant to be overridden */
  protected def shouldFlipRowOdd :Boolean = {
    return true
  }
  protected def getDataRow(oa: List[Any]): Node = {
    if (shouldFlipRowOdd)
      rowodd = ! rowodd
    val zip = keys.zip(oa)
    <tr> { (zip map ( (tuple: Tuple2[String,Any] ) => tuple match {
      case (key, v) if ! keysToIgnore.contains(key)  => getDataNodes(key, v, zip.toMap)
      case _ => NodeSeq.Empty
    } )).flatMap(x => x) }</tr> %
      new scala.xml.UnprefixedAttribute ( "class" ,
        if(rowodd){ "RowOdd" } else { "RowEven"} ,
        scala.xml.Null)
  }
  protected def getString(obj:Any) = {
    obj match {
      case null => ""
      case _ if obj.getClass == classOf[java.sql.Timestamp] =>
        obj.toString.split('.')(0) //we don't need to display fractional seconds
      case _ => obj.toString
    }
  }
  protected def getDataNodes(key: String, obj: Any, row: Map[String, Any] ): Seq[scala.xml.Node] = {
    <td> { getString(obj) } </td>  % {
      obj match{
        case _ if obj.isInstanceOf[Number] => new scala.xml.UnprefixedAttribute ("class", "number",   scala.xml.Null)
        case _ =>
          scala.xml.Null
      }
    }
  }

  def getTableContents: NodeSeq = {
    val nodeBuf = new scala.xml.NodeBuffer
    nodeBuf ++= getHeaderRow
    data.foreach( {row =>
        nodeBuf ++= getDataRow(row)
        nodeBuf ++= scala.xml.Text("\n")   }
    )
    //nodeBuf ++= data.map( oa =>  getDataRow(oa))
    //info("getTableContents: returning "+ nodeBuf)
    nodeBuf
  }
  def getTable(caption:Option[String]) = {
    <table style="border: 3px inset"> {
      caption.map(c => <caption>{ c }</caption>).getOrElse(scala.xml.Null)  ++
    getTableContents
      }</table>
  }
}



