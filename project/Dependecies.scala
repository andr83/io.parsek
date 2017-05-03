import sbt._


object Version {
  val cats = "0.9.0"
  val scalaTest = "3.0.1"
}

object Library {
  val cats = "org.typelevel" %% "cats" % Version.cats
  val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest % "test"
}