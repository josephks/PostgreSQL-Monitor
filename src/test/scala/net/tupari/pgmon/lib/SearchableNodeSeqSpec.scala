package net.tupari.pgmon.lib

import net.liftweb.common._
import org.specs2.mutable._
import   net.tupari.pgmon.lib.Implicits._

/**
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 2/28/12
 * Time: 11:08 PM
 */

class SearchableNodeSeqSpec  extends Specification{
    "A SearchableNode" should {
    "Find a top level node with a given attribute if it is there" in {
      <a id="123" />  searchForNodeWithAttrib( "id" -> "123") mustEqual Some(<a id="123" />)
      <b>some text</b><a id="123" /> <c />  searchForNodeWithAttrib( "id" -> "123") mustEqual Some(<a id="123" />)
    }
    "Not find a node with a given attribute if it is not there" in {
       <a id="123" />  searchForNodeWithAttrib( "id" -> "1234") mustEqual None
    }
    "Find a nested node  with a given attribute if it is there" in {
         <b><c><a id="123" /></c><d id="x"/></b>  searchForNodeWithAttrib( "id" -> "123") mustEqual Some(<a id="123" />)
    }
  }
}
