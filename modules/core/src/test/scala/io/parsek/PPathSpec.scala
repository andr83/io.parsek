package io.parsek

import org.scalatest.{FlatSpec, Matchers}
import io.parsek.PPath.root
import io.parsek.PValue._

/**
  * @author andr83
  */
class PPathSpec extends FlatSpec with Matchers {
  "PPath" should "focus" in {
    val value = pmap(
      "fBool" -> PBoolean(true),
      "fInt" -> PInt(10),
      "fLong" -> PLong(100L),
      "fDouble" -> PDouble(12.3),
      "fString" -> PString("hello")
    )
    root.at("fBool").boolean.getOption(value) shouldBe Some(true)
    root.at("fInt").int.getOption(value) shouldBe Some(10)
    root.at("fLong").long.getOption(value) shouldBe Some(100L)
    root.at("fDouble").double.getOption(value) shouldBe Some(12.3)
    root.at("fString").string.getOption(value) shouldBe Some("hello")
  }

  "PPath" should "focus with dynamics" in {
    val value = pmap(
      "fBool" -> PBoolean(true),
      "fInt" -> PInt(10),
      "fLong" -> PLong(100L),
      "fDouble" -> PDouble(12.3),
      "fString" -> PString("hello")
    )
    root.fBool.boolean.getOption(value) shouldBe Some(true)
    root.fInt.int.getOption(value) shouldBe Some(10)
    root.fLong.long.getOption(value) shouldBe Some(100L)
    root.fDouble.double.getOption(value) shouldBe Some(12.3)
    root.fString.string.getOption(value) shouldBe Some("hello")
  }
}
