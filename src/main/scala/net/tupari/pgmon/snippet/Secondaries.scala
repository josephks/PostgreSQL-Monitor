package net.tupari.pgmon.snippet

import xml.NodeSeq
import net.liftweb.common.Logger

import _root_.net.liftweb.util.BindHelpers._
import net.liftweb.http.{SHtml, S, DispatchSnippet}
import net.liftweb.util.Props

/**
 * <p>Snippet that creates a comet actor for each secondary in the config</p>
 *
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 3/26/12
 * Time: 6:15 PM
 */

class Secondaries extends DispatchSnippet with net.liftweb.common.LazyLoggable{
    def dispatch : DispatchIt = {
      case "allsecondaries" => allsecondaries _
    }

  //returns a new Node with attribs replaced
  private def setAttribsOn(node: scala.xml.Elem, attribs: List[(String, String)]): scala.xml.Elem = {
    attribs match{
      case Nil => node
      case (k, v) :: tail => setAttribsOn(node, tail) % new scala.xml.UnprefixedAttribute(k, v, scala.xml.Null)
    }
  }
  private def joinNodeSeqs( seq: Seq[NodeSeq]) : NodeSeq = {
    //In tests foldRight is slightly better than foldLeft, but using a NodeBuffer is orders of magnitudes faster than either
    val nb = new scala.xml.NodeBuffer
    seq.foreach( nb ++= _)
    nb  //implicit conversion changes to NodeSeq
       //seq.foldRight( scala.xml.NodeSeq.Empty) ( ( a:scala.xml.NodeSeq, b:scala.xml.NodeSeq) => a ++ b)
//    seq  match{
//      case Nil => NodeSeq.Empty
//      case head :: tail => head ++ joinNodeSeqs(tail)
//    }
  }

  private def single_secondary_outer( secName: String) = {
    ".secondary_actor" #>  { (nodes: scala.xml.NodeSeq) => {
      nodes.map( n => <div lift="comet?type=PgSecondaryActor" name={ secName }> { single_secondary(secName).apply(n.child)} </div> )
    } }
  }
  private def single_secondary(secName: String) = {
    ".secondary_name *+" #> secName &
    ".reptable" #>  { (node: scala.xml.NodeSeq) => {    //css selector of type Node => Node
      setAttribsOn(node.asInstanceOf[scala.xml.Elem], List( ("secondary" -> secName) )  )
    }   }
  }

  private def allSecondaryNames : List[String] = {
    //allow user to override the place we get the list of secondaries from
    val propname = Props.get("secondary.database.list.propname").getOrElse("secondary.database.names")
    val str = Props.get(propname).getOrElse("")
     logger.trace("allSecondaryNames(): parsing "+str+", props: "+Props.props)
    ( for ( rawname <- str.split(',') if rawname.trim.length > 0 )
    yield rawname.trim ).toList
  }

   def allsecondaries (xhtml : NodeSeq) = {
     //run xhtml through single_secondary_outer() for each secondary
         joinNodeSeqs( allSecondaryNames.map( secName =>
          single_secondary_outer( secName).apply(xhtml) )   )

  }


}
