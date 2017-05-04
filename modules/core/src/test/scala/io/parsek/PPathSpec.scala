package io.parsek

import org.scalatest.{FlatSpec, Matchers}
import io.parsek.PPath.root

/**
  * @author andr83
  */
class PPathSpec extends FlatSpec with Matchers {
  val testValue = pmap(
    "fBool" -> PValue(true),
    "fInt" -> PValue(10),
    "fLong" -> PValue(100L),
    "fDouble" -> PValue(12.3),
    "fString" -> PValue("hello"),
    "fArray" -> PValue(List(1, 2, 3))
  )

  "PPath" should "focus" in {
    root.at("fBool").boolean.getOption(testValue) shouldBe Some(true)
    root.at("fInt").int.getOption(testValue) shouldBe Some(10)
    root.at("fLong").long.getOption(testValue) shouldBe Some(100L)
    root.at("fDouble").double.getOption(testValue) shouldBe Some(12.3)
    root.at("fString").string.getOption(testValue) shouldBe Some("hello")
    root.at("fArray").arr.getOption(testValue) shouldBe Some(Vector(PValue(1), PValue(2), PValue(3)))
  }

  it should "focus with dynamics" in {
    root.fBool.boolean.getOption(testValue) shouldBe Some(true)
    root.fInt.int.getOption(testValue) shouldBe Some(10)
    root.fLong.long.getOption(testValue) shouldBe Some(100L)
    root.fDouble.double.getOption(testValue) shouldBe Some(12.3)
    root.fString.string.getOption(testValue) shouldBe Some("hello")
    root.fArray.arr.getOption(testValue) shouldBe Some(Vector(PValue(1), PValue(2), PValue(3)))
  }

  "orElse" should "return value from fallback path on primary fail" in {
    root.fStringWrong.string.get(testValue) shouldBe a [Left[_, _]]
    root.fStringWrong.orElse(root.fString).string.getOption(testValue) shouldBe Some("hello")
  }
}
