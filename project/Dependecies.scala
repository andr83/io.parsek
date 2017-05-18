import sbt._


object Version {
  val cats = "0.9.0"
  val jackson = "2.8.8"
  val scalaTest = "3.0.1"
}

object Library {
  val cats = "org.typelevel" %% "cats" % Version.cats
  val catsFree = "org.typelevel" %% "cats-free" % Version.cats
  val jackson = Seq(
    "com.fasterxml.jackson.core" % "jackson-core" % Version.jackson,
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % Version.jackson
  )
  val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest % "test"
}