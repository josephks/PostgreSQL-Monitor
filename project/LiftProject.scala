import sbt._

import sbt._
import Keys._

object MyBuild extends Build{

  val liftVersion = "2.4-M5"

//I'm not sure if these comments are still valid for sbt 0.10.x:
  // uncomment the following if you want to use the snapshot repo
  //  val scalatoolsSnapshot = ScalaToolsSnapshots

  // If you're using JRebel for Lift development, uncomment
  // this line
  // override def scanDirectories = Nil

  lazy val JavaNet = "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

   lazy val root = Project("root", file(".")) settings(

    libraryDependencies ++= Seq(
      "net.liftweb" %% "lift-webkit" % liftVersion % "compile",
      "net.liftweb" %% "lift-mapper" % liftVersion % "compile",
      "org.mortbay.jetty" % "jetty" % "6.1.26" % "container",
      "junit" % "junit" % "4.7" % "test",
      "ch.qos.logback" % "logback-classic" % "0.9.26",
      "org.scala-tools.testing" %% "specs" % "1.6.9" % "test",
      "com.h2database" % "h2" % "1.2.147",
      "org.specs2" %% "specs2" % "1.6.1",
      "org.specs2" %% "specs2-scalaz-core" % "6.0.1" % "test"

      //  "org.scala-tools.testing" %% "scalacheck" % "1.9", 
      //  "org.scala-tools.testing" % "test-interface" % "0.5", 
      //  "org.hamcrest" % "hamcrest-all" % "1.1",
      //  "org.mockito" % "mockito-all" % "1.8.5",
    )

  )
}
