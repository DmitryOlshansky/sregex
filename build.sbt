version := "0.1"

scalaVersion := "2.11.6"

name := "Simple regex"

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
	"org.scalactic" %% "scalactic" % "3.0.0",
	"org.scalatest" %% "scalatest" % "3.0.0" % "test",
	"com.ibm.icu" % "icu4j" % "58.2"
)