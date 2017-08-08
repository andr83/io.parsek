import sbt._

organization in ThisBuild := "io.parsek"

scalaVersion in ThisBuild := "2.11.11"

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

def parsekModule(path: String): Project = {
  val id = path.split("-").reduce(_ + _.capitalize)
  Project(id, file(s"modules/$path"))
    .settings(
      moduleName := s"parsek-$path",
      name := s"Parsek $id",
      credentials += Credentials(
        realm = "Artifactory Realm",
        host = "rms.evolutiongaming.com",
        userName = "atupitsin",
        passwd = "1234mudar@"
      ),
      publishMavenStyle := true,

      publishTo in ThisBuild := Some("Artifactory Realm" at "https://rms.evolutiongaming.com/mvn-spark")
    )
}