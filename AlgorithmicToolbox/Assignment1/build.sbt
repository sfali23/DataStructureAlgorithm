name := "AlgorithmicToolboxAssignment1"
organization := "com.alphasystem"

scalaVersion := "2.12.3"

lazy val `AlgorithmicToolboxAssignment1` = project in file(".")

libraryDependencies ++= {
  Seq("org.scalatest" %% "scalatest" % "3.0.3" % "test")
}

//Scalafmt - Code Formatter
scalafmtOnCompile in ThisBuild := true // all projects
scalafmtOnCompile := true // current project
scalafmtOnCompile in Compile := true // current project, specific configuration
