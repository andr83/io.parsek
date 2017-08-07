package io.parsek.optics

import io.parsek.implicits._
import io.parsek.{PValue, pmap}
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Andrei Tupitcyn
  */
class PValueSyntaxSpec extends FlatSpec with Matchers {
  private val testValue = pmap(
    'fBool -> PValue(true),
    'fInt -> PValue(10),
    'fLong -> PValue(100L),
    'fDouble -> PValue(12.3),
    'fString -> PValue("hello"),
    'fArray -> PValue(List(1, 2, 3))
  )

  "PValue" should "decoded directly" in {
    val expected = "some string"
    PValue.fromString(expected).as[String] shouldBe Right(expected)
  }

  it should "traverse throw PMap" in {
    testValue.at('fBool).boolean shouldBe true
    testValue.at('fInt).int shouldBe 10
    testValue.at('fLong).long shouldBe 100L
    testValue.at('fDouble).double shouldBe 12.3
    testValue.at('fString).string shouldBe "hello"
    testValue.at('fArray).arr[Int] shouldBe List(1, 2, 3)
  }

  it should "traverse throw PMap with Dynamic support" in {
    testValue.fBool.boolean shouldBe true
    testValue.fInt.int shouldBe 10
    testValue.fLong.long shouldBe 100L
    testValue.fDouble.double shouldBe 12.3
    testValue.fString.string shouldBe "hello"
    testValue.fArray.arr[Int] shouldBe List(1, 2, 3)
  }

  it should "apply unsafe map function" in {
    testValue.fInt.mapUnsafe((i: Int) => i * 2).int shouldBe 20
    testValue.fArray.mapUnsafe[Int, Int](i => i * 3).arr[Int] shouldBe List(3, 6, 9)
    testValue.mapK[Boolean, String]((k, v) => k -> v.toString).value shouldBe testValue.update('fBool, PValue("true"))
  }
}
