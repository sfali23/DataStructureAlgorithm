name := "AlgorithmicToolboxAssignment4"
organization := "com.alphasystem"

scalaVersion := "2.12.4"

lazy val `AlgorithmicToolboxAssignment4` = project in file(".")

libraryDependencies ++= {
  Seq("org.scalatest" %% "scalatest" % "3.0.3" % Test,
      "junit" % "junit" % "4.10" % Test)
}

//Scalafmt - Code Formatter
scalafmtOnCompile in ThisBuild := true // all projects
scalafmtOnCompile := true // current project
scalafmtOnCompile in Compile := true // current project, specific configuration
