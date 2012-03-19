package net.tupari.pgmon.lib

import org.specs2.mutable._


/**
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 3/14/12
 * Time: 1:35 PM
 */

class ConnectionDataSpec extends Specification {
  val url_with_expected = List( ("jdbc:postgresql:database" -> ("127.0.0.1", ConnectionData.default_port)),
    ("jdbc:postgresql://localhost/database" -> ("127.0.0.1", ConnectionData.default_port)),
    ("jdbc:postgresql://localhost:5555/database" -> ("127.0.0.1", 5555)),
    ("jdbc:postgresql://127.0.0.2/database" -> ("127.0.0.2", ConnectionData.default_port)),
    ("jdbc:postgresql://127.0.0.2:5555/database" -> ("127.0.0.2", 5555)) )

  "ConnectionData.getHost()" should {
    //this produces:
//[error] /home/jks/work/pgmon/src/test/scala/net/tupari/pgmon/lib/ConnectionDataSpec.scala:24: type mismatch;
//[error]  found   : List[org.specs2.specification.Example]
//[error]  required: org.specs2.specification.Example
//[error]     url_with_expected.map{
//
//    url_with_expected.map{
//      case (input, (host, port)) =>
//        "parse jdbc uri"+input+" as expected" in {
//          ConnectionData.getHost(input) mustEqual (host, port)
//        }
//    }
    "parse jdbc uris as expected" in {
      url_with_expected.map{
        case (input, (host, port)) =>
          ConnectionData.getHost(input) mustEqual (host, port)
      }
    }
  } //should
}

