import sbt._


object Version {
  val calcite = "1.12.0"
  val cats = "0.9.0"
  val scalaTest = "3.0.1"
}

object Library {
  val calcite =  "org.apache.calcite" % "calcite-core" % Version.calcite
  val calciteLinq4j =  "org.apache.calcite" % "calcite-linq4j" % Version.calcite
  val cats = "org.typelevel" %% "cats" % Version.cats
  val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest % "test"
}