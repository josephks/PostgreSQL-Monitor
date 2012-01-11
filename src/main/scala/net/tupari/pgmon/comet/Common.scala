package net.tupari.pgmon.comet

import net.liftweb.common.{Logger, Full}
import xml.{NodeSeq, Node}

/**
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 1/7/12
 * Time: 9:11 PM
 * To change this template use File | Settings | File Templates.
 */

object Common {

  //returns an either (tuple, error string)
  def getData(q: String, target_host: String = null, cur_name: String = null): Either[String, Tuple2[ List[String],List[List[Any]] ] ] = {
    try{
      Right(net.liftweb.db.DB.performQuery(q)   )
    }catch{
      //I don't know why but a query that returns no data is considered an error
      case ex: org.postgresql.util.PSQLException  if (ex.getMessage.contains("No results were returned by the query."))   =>
        Right(Nil, Nil)

    }
    //todo: catch exception, return Left, or convert to use Box
  }

}



