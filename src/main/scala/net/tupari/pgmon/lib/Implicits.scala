package net.tupari.pgmon.lib

/**  <p>Implicity conversion live here.</p>
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 2/28/12
 * Time: 10:53 PM
 * To change this template use File | Settings | File Templates.
 */

object Implicits {

  implicit def nodeToSearchableNode(nodeseq:scala.xml.NodeSeq )  = {
       new SearchableNodeSeq(nodeseq)
  }
  implicit def nodeToSearchableNode(nodebuf:scala.xml.NodeBuffer )  = {
       new SearchableNodeSeq(nodebuf)
  }
}
