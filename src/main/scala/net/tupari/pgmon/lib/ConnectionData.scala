package net.tupari.pgmon.lib

import net.liftweb.db.{DefaultConnectionIdentifier, ConnectionIdentifier}
/**
 * <p>Utility that stores host/port info for db connections</p>
 *
 * User: jks
 * Date: 3/13/12
 * Time: 1:30 PM
 */

object ConnectionData {
  val default_port = 5432
   val uriRegex = """//([\.\w]+)(:\d+)?/.*""".r


  var ipmap:Map[ConnectionIdentifier, (String,  Int)] = Map()
  
  def addUri(ci: ConnectionIdentifier, uri: String){
    ipmap += (ci -> getHost(uri))
  }

  def getHost(uri: String):(String,  Int) = {
    //see: http://jdbc.postgresql.org/documentation/head/connect.html
    if (! uri.startsWith("jdbc:postgresql:"))
      throw new IllegalArgumentException("This is not a postgresql jdbc uri: "+uri)
    val uri2 = uri.substring("jdbc:postgresql:".length())
    if (! uri2.contains("/"))
      return ("127.0.0.1", default_port)
    uri2 match{
      case uriRegex(host, port) =>
        val rport = port match { case null => default_port case _ => port.substring(1).toInt }
        return (java.net.InetAddress.getByName(host).getHostAddress() , rport)
    }


  }

}
