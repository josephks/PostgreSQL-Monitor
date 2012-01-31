package bootstrap.liftweb

import net.liftweb._
import db.PostgreSqlDriver
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import mapper._

import code.model._
import net.tupari.pgmon.schemagen.SchemaPrinter


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends LazyLoggable{
  def boot {
    logger.info("Boot starting!")

    // where to search snippet
    LiftRules.addToPackages("net.tupari.pgmon")
      logger.info("Boot setting sitemap")
    // Build SiteMap
    def sitemap = SiteMap(
      Menu.i("Home") / "index",
      Menu("Backends") / "pgmon",
      Menu("Size") / "pgsize",

      // more complex because this menu allows anything in the
      // /static path to be visible
      Menu(Loc("Static", Link(List("static"), true, "/static/index"), 
	       "Static Content")))

    def sitemapMutators = User.sitemapMutator

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    //LiftRules.setSiteMapFunc(() => sitemapMutators(sitemap))


    LiftRules.dispatch.append {
      case Req("api" :: "schema" :: Nil , _, _) =>
        () => SchemaPrinter.getResponse(None)
      case Req("api" :: "schema" :: format :: Nil, _, _) =>
        () => SchemaPrinter.getResponse(Some(format))
    }

    // Use jQuery 1.4
    LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)

    logger.info("Boot: setting db connection")
    Class.forName("org.postgresql.Driver")
    if (!DB.jndiJdbcConnAvailable_?) {
      //Creator a vendor class that extends StandardDBVendor.
      //The point of this is to set the appliation_name in pg 9 and above
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.postgresql.Driver",
          Props.get("db.url") openOr  "jdbc:postgresql://localhost/template1",
          Props.get("db.user"), Props.get("db.password")) {
          override def createOne: Box[java.sql.Connection] =  {
            val ans = super.createOne
            ans match{
              case Full(conn) =>
                val meta = conn.getMetaData
                (meta.getDatabaseProductName,meta.getDatabaseMajorVersion) match {
                  case (PostgreSqlDriver.name, major) if (major >= 9 ) =>
                    val st = conn.createStatement
                    try {
                      st.execute("SET application_name = 'PgMon'")
                    }catch{
                      case ex => logger.warn(ex)
                    }finally{
                      st.close
                    }
                    case _ =>
                }
              case _ =>
            }
            ans
          }
        }

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    logger.info("Boot done")
  }
}
