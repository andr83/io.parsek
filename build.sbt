import sbt.Keys.scalaVersion
import sbt.{ThisBuild, _}


crossScalaVersions := Seq("2.10.6", "2.11.12", "2.12.4")

lazy val core = parsekModule("core")
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
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

lazy val shapeless = parsekModule("shapeless")
  .settings(
    libraryDependencies ++=
      Library.shapeless ++
      Seq(
        Library.scalaTest
      )
  )
  .dependsOn(core)

def parsekModule(path: String): Project = {
  val id = path.split("-").reduce(_ + _.capitalize)
  Project(id, file(s"modules/$path"))
    .settings(
      organization := "io.parsek",
      moduleName := s"parsek-$path",
      name := s"Parsek $id",
      crossScalaVersions := Seq("2.10.6", "2.11.12", "2.12.4"),
      ThisBuild / scalaVersion := "2.12.4",
      Compile / scalacOptions ++= Seq(
        "-deprecation",
        "-unchecked",
        "-encoding",
        "utf-8"
      ),
      scalacOptions += "-feature"
    )
}
