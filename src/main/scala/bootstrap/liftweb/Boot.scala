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
import net.tupari.pgmon.lib.Implicits._
import java.net.URL


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends LazyLoggable{
  def boot {
    logger.info("Boot starting!")
    //Leave remmed out unless needed.  Otherwise might leak passwords into the log file
    //logger.info("Props: "+Props.props+" from file " + Props.propFileName+" totry: " + Props.toTry.map(_.apply()) )


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
    //Class.forName("org.postgresql.Driver")
    Class.forName(classOf[org.postgresql.Driver].getName)

    //private method for getting db vendors
    def getDbVendor(url: String, user: Box[String], passwd: Box[String]): mapper.StandardDBVendor = {
      //Creator a vendor class that extends StandardDBVendor.
      //Lift has this weird thing that if the user is set but the passwd isn't it won't use the user
      //If user is a Full but passwd isn't set passwd to be Full("")
      val passwd_to_use = passwd match{
        case Full(_) => passwd
        case _ => user match{
          case Full(_) => Full("")
          case _ =>  passwd
        }
      }
      new StandardDBVendor(Props.get("db.driver") openOr "org.postgresql.Driver",
        url, user, passwd_to_use) {
        override def createOne: Box[java.sql.Connection] = {
          val ans = super.createOne
          ans match {
            case Full(conn) =>
              val meta = conn.getMetaData
              (meta.getDatabaseProductName, meta.getDatabaseMajorVersion) match {
                case (PostgreSqlDriver.name, major) if (major >= 9) =>
                  val st = conn.createStatement
                  try {
                    //The point of this is to set the appliation_name in pg 9 and above
                    st.execute("SET application_name = 'PgMon'")
                  } catch {
                    case ex => logger.warn(ex)
                  } finally {
                    st.close
                  }
                case _ =>
              }
            case _ =>
          }
          ans
        }
      }
    } //def
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = getDbVendor(Props.get("db.url") openOr "jdbc:postgresql://localhost/template1", Props.get("db.user"), Props.get("db.password"))
      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }
    val doneNames = scala.collection.mutable.Set[String](DefaultConnectionIdentifier.jndiName)
    for (str <- Props.get("secondary.database.names") ;
         rawname <- str.split(',')){
           val name = rawname.trim
      if (doneNames.contains(name)){
        logger.error("There already is a db connection named '"+name+"'")
      } else if (name.length() > 0){
        Props.get("db."+name+".url") match{
          case Full(uri) =>
            val vendor = getDbVendor(uri, Props.get("db."+name+".user"), Props.get("db."+name+".password"))
            LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)
            DB.defineConnectionManager(name, vendor)
            try{
              net.tupari.pgmon.lib.ConnectionData.addUri(name, uri )
            }catch{
              case e => logger.error("could not determine ip addr for seconary db  '"+name+"'", e)
            }
          case _ => logger.error("secondary db '"+name+"' does not have a url configured")
        }
        doneNames += name
      }
    }  //for
    if (doneNames.size > 1)
      logger.info("created connections for these secondary dbs: "+doneNames+" ipmap: "+net.tupari.pgmon.lib.ConnectionData.ipmap )


    net.liftweb.widgets.flot.Flot.init

    import net.liftweb.http.ResourceServer
    ResourceServer.allow({
      case "flot" :: "jquery.flot.resize.js" :: Nil => true
    })

    logger.info("Boot done")
  }
}
