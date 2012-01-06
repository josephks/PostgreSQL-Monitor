package net.tupari.lib

import net.liftweb.util._
import net.liftweb.http.Factory
import scala.reflect.Manifest


object SimpFactory extends SimpleInjector {

  type UniqueNumber = Int

  private val atom = new java.util.concurrent.atomic.AtomicInteger

//to use: SimpFactory.inject[ SimpFactory.UniqueNumber]
  registerInjection( () => atom.getAndIncrement().asInstanceOf[UniqueNumber]  )

  registerInjection(  () => new java.util.Date ) 


}
