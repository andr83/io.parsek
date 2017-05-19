import sbt._


object Version {
  val cats = "0.9.0"
  val catsEffect = "0.2"
  val jackson = "2.8.8"
  val h2 = "1.4.195"
  val scalaTest = "3.0.1"
}

object Library {
  val cats = "org.typelevel" %% "cats" % Version.cats
  val catsFree = "org.typelevel" %% "cats-free" % Version.cats
  val catsEffect = "org.typelevel" %% "cats-effect" % Version.catsEffect
  val jackson = Seq(
    "com.fasterxml.jackson.core" % "jackson-core" % Version.jackson,
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % Version.jackson
  )
  val h2 =  "com.h2database" % "h2" % Version.h2
  val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest % "test"
}