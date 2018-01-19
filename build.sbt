import sbt.Keys.scalaVersion
import sbt._
import ReleaseTransformations._

val scalacOpts = Seq(
  "-encoding",
  "UTF-8",
  "-deprecation", // warning and location for usages of deprecated APIs
  "-feature", // warning and location for usages of features that should be imported explicitly
  "-language:postfixOps",
  "-language:higherKinds",             // Allow higher-kinded types
  "-language:implicitConversions",
  "-unchecked", // additional warnings where generated code depends on assumptions
  "-Xlint", // recommended additional warnings
  "-Xcheckinit", // runtime error when a val is not initialized due to trait hierarchies (instead of NPE somewhere else)
  "-Xfatal-warnings", // all warnings become errors
  "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
  "-Ywarn-inaccessible",
  "-Ywarn-dead-code",
  //"-Ywarn-unused-import"
)

lazy val root = project
  .in(file("."))
  .aggregate(
    core,
    jackson,
    shapeless,
    jdbc
  )
  .settings(
    skip in publish := true
  )

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
      scalaVersion := "2.12.4",
      scalacOptions ++= scalacOpts,
      scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
      releaseCrossBuild := true,
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseProcess := Seq[ReleaseStep](
        checkSnapshotDependencies,
        inquireVersions,
        runClean,
        runTest,
        setReleaseVersion,
        commitReleaseVersion,
        tagRelease,
        publishArtifacts,
        setNextVersion,
        commitNextVersion,
        releaseStepCommand("sonatypeReleaseAll"),
        pushChanges
      ),
      homepage := Some(url("https://github.com/andr83/io.parsek")),
      scmInfo := Some(ScmInfo(url("https://github.com/andr83/io.parsek"),
                        "git@github.com:andr83/io.parsek.git")),
      developers := List(
        Developer("andr83", "Andrei Tupitcyn", "andrew.tupitsin@gmail.com", url("https://github.com/andr83")),
        Developer("fabura", "Bulat Fattahov", "fabura@yandex.ru", url("https://github.com/fabura"))
      ),
      licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
      publishMavenStyle := true,
      publishTo := Some(
        if (isSnapshot.value)
          Opts.resolver.sonatypeSnapshots
        else
          Opts.resolver.sonatypeStaging
      )
    )
}
