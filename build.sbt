/** Project */
name := "PostgreSQL Monitor"

version := "0.1.1"

organization := "net.tupari"

scalaVersion := "2.9.1"

//for web plugin
//seq(webSettings :_*)




/** Shell */
shellPrompt := { state => System.getProperty("user.name") + " " +System.getProperty("user.dir") + " sbt> " }

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

/** Dependencies */
resolvers ++= Seq(
                 //"snapshots-repo" at "http://scala-tools.org/repo-snapshots", 
                  "Local Maven Repository" at "file://$M2_REPO")




/** Compilation */
javacOptions ++= Seq("-Xmx1812m", "-Xms512m", "-Xss4m")

scalacOptions += "-deprecation"

maxErrors := 20

pollInterval := 1000

testFrameworks += new TestFramework("org.specs2.runner.SpecsFramework")

testOptions := Seq(Tests.Filter(s =>
  Seq("Spec", "Suite", "Unit", "Specs", "Test", "all").exists(s.endsWith(_)) &&
    ! s.endsWith("FeaturesSpec") ||
    s.contains("UserGuide") || 
    s.matches("org.specs2.guide.*")))

/** Console */
initialCommands in console := "import org.specs2._"
