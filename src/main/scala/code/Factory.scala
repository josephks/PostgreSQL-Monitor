
import net.liftweb.util._
import net.liftweb.http.Factory
import scala.reflect.Manifest


object SimpFactory extends SimpleInjector {

  type UniqueNumber = Int

  private val atom = new java.util.concurrent.atomic.AtomicInteger


  registerInjection( () => atom.getAndIncrement().asInstanceOf[UniqueNumber]  )

  registerInjection(  () => new java.util.Date ) 


}
