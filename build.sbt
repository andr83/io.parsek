import sbt._

organization in ThisBuild := "io.parsek"

scalaVersion in ThisBuild := "2.11.11"

crossScalaVersions := Seq("2.11.11", "2.12.4")

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
      Library.scalaArm,
      Library.h2 % "test",
      Library.scalaTest
    )
  )
  .dependsOn(core)

lazy val calcite = parsekModule("calcite")
  .settings(
    libraryDependencies ++= Seq(
      Library.calcite,
      Library.calciteLinq4j,
      Library.scalaTest,
      Library.scalaMeter
    )
  )
  .dependsOn(core, jdbc)


lazy val shapeless = parsekModule("shapeless")
  .settings(
    libraryDependencies ++= Seq(
      Library.shapeless,
      Library.scalaTest
    )
  )
  .dependsOn(core)

def parsekModule(path: String): Project = {
  val id = path.split("-").reduce(_ + _.capitalize)
  Project(id, file(s"modules/$path"))
    .settings(
      moduleName := s"parsek-$path",
      name := s"Parsek $id"
    )
}
