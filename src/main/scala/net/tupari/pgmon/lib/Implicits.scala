package net.tupari.pgmon.lib

/**  <p>Implicit conversions live here.</p>
 * User: jks
 * Date: 2/28/12
 * Time: 10:53 PM
 */

object Implicits {

  implicit def nodeToSearchableNode(nodeseq:scala.xml.NodeSeq )  = {
       new SearchableNodeSeq(nodeseq)
  }
  implicit def nodeToSearchableNode(nodebuf:scala.xml.NodeBuffer )  = {
       new SearchableNodeSeq(nodebuf)
  }

  implicit def stringToConnectionIdentifier(name: String) = {
    name match{
      case null => null
      case _ =>
        new net.liftweb.db.ConnectionIdentifier {
          val jndiName = name
        }
    } //match
  }
}
