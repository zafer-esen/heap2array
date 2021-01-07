import Dependencies._

lazy val commonSettings = Seq(
  name := "heap2array",
  organization := "uuverifiers",
  version := "0.1.0-SNAPSHOT",
  homepage := Some(url("https://github.com/uuverifiers/heap2array")),
  //licenses := Seq("BSD License 2.0" -> url("https://github.com/uuverifiers/heap2arrayblob/master/LICENSE")),
  scalaVersion := "2.11.12",
  crossScalaVersions := Seq("2.11.12", "2.12.10"),
  fork in run := true,
  cancelable in Global := true//,
  //publishTo := Some(Resolver.file("file",  new File( "/home/wv/public_html/maven/" )) )
)

lazy val root = (project in file("."))
  .settings(
    name := "heap2array",
    scalacOptions in Compile ++=
      List("-feature",
           "-language:implicitConversions,postfixOps,reflectiveCalls"),
    //
    mainClass in Compile := Some("heap2array.Main"),
    //
    // libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value,
    resolvers += "uuverifiers" at "http://logicrunch.research.it.uu.se/maven/",
    libraryDependencies += "uuverifiers" %% "princess" % "heap-SNAPSHOT",
    libraryDependencies += "uuverifiers" %% "princess-smt-parser" % "heap-SNAPSHOT"
  )

