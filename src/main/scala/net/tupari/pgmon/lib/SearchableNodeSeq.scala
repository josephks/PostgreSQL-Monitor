package net.tupari.pgmon.lib

/**  <p>A pimped NodeSeq</p>
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 2/28/12
 * Time: 10:53 PM
 */



class SearchableNodeSeq(nodeseq:scala.xml.NodeSeq) {

  /** Find first node with this attribute */
  def searchForNodeWithAttrib( tup: (String, String)): Option[scala.xml.Node] = {
    searchForNodeWithAttrib0(  nodeseq, tup)
  }
  private def searchForNodeWithAttrib0(nodeseq:scala.xml.NodeSeq,  tup: (String, String)): Option[scala.xml.Node] = {
    tup match{
      case (name, value) if (name != null && value != null && name.length() > 0 && value.length() > 0) =>
        for(node <- nodeseq){
          if ( (node \ ("@"+name)).text == value)
            return Some(node)
          for (found <- searchForNodeWithAttrib0(node.descendant, tup).find( p => (p \ ("@"+name)).text == value) ){
            return Some(found)
          }
        }
        return None
    }
  }
}
