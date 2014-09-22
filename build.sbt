name := """ThePhinisher"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "mysql" % "mysql-connector-java" % "5.1.30",
  "org.webjars" % "bootstrap" % "3.2.0",
  "org.webjars" %% "webjars-play" % "2.3.0",
  "org.webjars" % "bootstrap-tokenfield" % "0.12.0",
  "org.webjars" % "typeaheadjs" % "0.10.4-1",
  "ws.securesocial" %% "securesocial" % "master-SNAPSHOT",
  ws
)
 
resolvers += "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
