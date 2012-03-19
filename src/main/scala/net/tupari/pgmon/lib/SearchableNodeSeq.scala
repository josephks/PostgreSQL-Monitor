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
  //todo: instead of returning a Node, return something that could be implicity be turned into a Node, or
  //could be used in other ways
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
  //todo: do something more DSL like, for example: {@code val newxml =  <xml /> searchForNodeWithAttrib("x", "y") replaceWith <newnode /> }
  def replaceNodeWith(target:scala.xml.Node, replacement:scala.xml.Node ): scala.xml.NodeSeq = {
    import xml.transform.{RewriteRule, RuleTransformer}
    object NodeReplacer  extends RewriteRule {
      override def transform(n: scala.xml.Node): Seq[scala.xml.Node] ={
        if (n == target) {
          replacement
        } else n
      }
    }
    new RuleTransformer(NodeReplacer).transform(nodeseq)
  }
}
