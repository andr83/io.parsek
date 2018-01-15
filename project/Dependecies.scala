import sbt._


object Version {
  val jackson = "2.8.8"
  val h2 = "1.4.195"
  val paradisePlugin = "2.1.0"
  val scalaArm = "2.0"
  val scalaTest = "3.0.4"
  val shapeless = "2.3.3"
}

object Library {
  val jackson = Seq(
    "com.fasterxml.jackson.core" % "jackson-core" % Version.jackson,
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % Version.jackson
  )

  val h2 =  "com.h2database" % "h2" % Version.h2

  val scalaArm = "com.jsuereth" %% "scala-arm"% Version.scalaArm
  val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest % "test"
  val shapeless = Seq(
    "com.chuusai" %% "shapeless" % Version.shapeless,
    compilerPlugin("org.scalamacros" % "paradise" % Version.paradisePlugin cross CrossVersion.full)
  )
}