import sbt._


object Version {
  val calcite = "1.12.0"
  val cats = "0.9.0"
  val catsEffect = "0.2"
  val jackson = "2.8.8"
  val h2 = "1.4.195"
  val scalaArm = "2.0"
  val scalaMeter = "0.8.2"
  val scalaTest = "3.0.1"
}

object Library {
  val calcite = "org.apache.calcite" % "calcite-core" % Version.calcite
  val calciteLinq4j = "org.apache.calcite" % "calcite-linq4j" % Version.calcite
  val cats = "org.typelevel" %% "cats" % Version.cats
  val catsFree = "org.typelevel" %% "cats-free" % Version.cats
  val catsEffect = "org.typelevel" %% "cats-effect" % Version.catsEffect
  val jackson = Seq(
    "com.fasterxml.jackson.core" % "jackson-core" % Version.jackson,
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % Version.jackson
  )
  val scalaArm = "com.jsuereth" % "scala-arm_2.11"% Version.scalaArm

  val h2 =  "com.h2database" % "h2" % Version.h2
  val scalaMeter = "com.storm-enroute" % "scalameter_2.11" % Version.scalaMeter % "test"
  val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest % "test"
}