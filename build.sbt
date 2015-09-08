name := "reflection"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-deprecation"
)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "compile",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)
