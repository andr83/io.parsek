import sbt._

organization in ThisBuild := "io.parsek"

scalaVersion := "2.11.11"

def parsekModule(path: String): Project = {
  val id = path.split("-").reduce(_ + _.capitalize)
  Project(id, file(s"modules/$path"))
    .settings(
      moduleName := "parsek-$name",
      name := "Parsek $name"
    )
}

lazy val core = parsekModule("core")
  .settings(
    libraryDependencies ++= Seq(
      Library.cats,
      Library.scalaTest
    )
  )