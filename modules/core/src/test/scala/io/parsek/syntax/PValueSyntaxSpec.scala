package io.parsek.syntax

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import io.parsek.implicits._
import io.parsek.{PResult, PValue, pmap}
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Andrei Tupitcyn
  */
class PValueSyntaxSpec extends FlatSpec with Matchers {
  private val today = LocalDate.now()

  private val testValue = pmap(
    'fBool -> PValue(true),
    'fInt -> PValue(10),
    'fLong -> PValue(100L),
    'fDouble -> PValue(12.3),
    'fString -> PValue("hello"),
    'fDate -> PValue.apply(today),
    'fArray -> PValue(List(1, 2, 3))
  )

  "PValue" should "decoded directly" in {
    val expected = "some string"
    PValue.fromString(expected).as[String] shouldBe PResult.valid(expected)
  }

  it should "traverse throw PMap" in {
    testValue.at('fBool).to[Boolean] shouldBe true
    testValue.at('fInt).to[Int] shouldBe 10
    testValue.at('fLong).to[Long] shouldBe 100L
    testValue.at('fDouble).to[Double] shouldBe 12.3
    testValue.at('fString).to[String] shouldBe "hello"
    testValue.at('fDate).to[LocalDate] shouldBe today
    testValue.at('fDate).to[String] shouldBe DateTimeFormatter.ISO_LOCAL_DATE.format(today)
    testValue.at('fArray).to[List[Int]] shouldBe List(1, 2, 3)
  }

  it should "traverse throw PMap with Dynamic support" in {
    testValue.fBool.to[Boolean] shouldBe true
    testValue.fInt.to[Int] shouldBe 10
    testValue.fLong.to[Long] shouldBe 100L
    testValue.fDouble.to[Double] shouldBe 12.3
    testValue.fString.to[String] shouldBe "hello"
    testValue.fDate.to[LocalDate] shouldBe today
    testValue.fArray.to[Vector[Int]] shouldBe List(1, 2, 3)
  }

  it should "apply unsafe map function" in {
    testValue.fInt.map((i: Int) => i * 2).unsafe.to[Int] shouldBe 20
    testValue.fArray.mapValues[Int, Int](i => i * 3).unsafe.to[Vector[Int]] shouldBe List(3, 6, 9)
    testValue.mapWithKey[Boolean, String]((k, v) => k -> v.toString).unsafe shouldBe testValue.update('fBool, PValue("true"))
  }

  "Any type with available encoder" should "convert to PValue" in {
    10.toPValue shouldBe PValue(10)
  }
}
