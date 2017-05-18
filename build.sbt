import sbt._

organization in ThisBuild := "io.parsek"

scalaVersion in ThisBuild := "2.11.11"

def parsekModule(path: String): Project = {
  val id = path.split("-").reduce(_ + _.capitalize)
  Project(id, file(s"modules/$path"))
    .settings(
      moduleName := s"parsek-$path",
      name := s"Parsek $id"
    )
}

lazy val core = parsekModule("core")
  .settings(
    libraryDependencies ++= Seq(
      Library.cats,
      Library.scalaTest
    )
  )

lazy val jackson = parsekModule("jackson")
  .settings(
    libraryDependencies ++= Library.jackson,
    libraryDependencies += Library.scalaTest
  )
  .dependsOn(core)

lazy val jdbc = parsekModule("jdbc")
  .settings(
    libraryDependencies ++= Seq(
      Library.catsFree,
      Library.scalaTest
    )
  )
  .dependsOn(core)