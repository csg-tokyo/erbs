lazy val root = Project(
  id = "erbs",
  base = file(".")
).settings(
  name := "erbs",
  version := "0.0.1",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "3.0.0",
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  )
)

mainClass in Compile := Some("erbs.Runner")
