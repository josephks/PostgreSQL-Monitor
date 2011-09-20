import net.liftweb.common._
import org.specs2.mutable._

class FactorySpec extends Specification {

  "A simple factory" should {
    "Give unique numbers" in {

      val zero = SimpFactory.inject[ SimpFactory.UniqueNumber]
      val one = SimpFactory.inject[ SimpFactory.UniqueNumber]
      zero must beLike{
        case Full(x) => one must beLike {
          case Full(y) => x mustNotEqual y
        }
      }
    }
    "Get the current date" in {
      val d =  SimpFactory.inject[ java.util.Date]
      d must beLike {
        case Full(x) => x must haveClass[java.util.Date]
      }
    }
  }

}// FactorySpec
