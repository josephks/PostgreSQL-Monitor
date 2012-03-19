package net.tupari.pgmon.comet

import net.liftweb.common.{Logger, Full}
import xml.{NodeSeq, Node}
import net.tupari.pgmon.lib.Implicits
import net.liftweb.db.{DefaultConnectionIdentifier, ConnectionIdentifier}

/**
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 1/7/12
 * Time: 9:11 PM
 */

object Common {

  //returns an either (tuple, error string)
    def getData(q: String, target_host: String = null, cur_name: String = null): Either[String, Tuple2[ List[String],List[List[Any]] ] ] = {
      val connectionIdentifier = Implicits.stringToConnectionIdentifier(target_host)
      getDataFromConnection(q, Option(connectionIdentifier).getOrElse(DefaultConnectionIdentifier), cur_name)
    }
  def getDataFromConnection(q: String, target_host: ConnectionIdentifier , cur_name: String = null): Either[String, Tuple2[ List[String],List[List[Any]] ] ] = {
    try{
      Right(net.liftweb.db.DB.performQuery(q, Nil, target_host)   )
    }catch{
      //I don't know why but a query that returns no data is considered an error
      case ex: org.postgresql.util.PSQLException  if (ex.getMessage.contains("No results were returned by the query."))   =>
        Right(Nil, Nil)

    }
    //todo: catch exception, return Left, or convert to use Box
  }

  def addResizePluginToFlotWidget(flot_widget_rendered: scala.xml.NodeSeq)={
      import xml.transform.{RewriteRule, RuleTransformer}
      val refseq =  <script type="text/javascript" src="/classpath/flot/jquery.flot.js"></script>
      object FixFlotSrc  extends RewriteRule {
        override def transform(n: scala.xml.Node): Seq[scala.xml.Node] ={
          if (n == refseq) {  //when we find the <script> for jquery.flot.js append <script> for resize plugin right after
            n ++ <script type="text/javascript" src="/classpath/flot/jquery.flot.resize.js"></script>
          } else n
        }
      }
       new RuleTransformer(FixFlotSrc).transform(flot_widget_rendered)
  }

}



