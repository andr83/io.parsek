package io.parsek.optics

import io.parsek._
import io.parsek.implicits._
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author andr83
  */
class PPathSpec extends FlatSpec with Matchers {
  val testValue = pmap(
    'fBool -> PValue(true),
    'fInt -> PValue(10),
    'fLong -> PValue(100L),
    'fDouble -> PValue(12.3),
    'fString -> PValue("hello"),
    'fArray -> PValue(List(1, 2, 3))
  )

  "PPath" should "focus" in {
    root.at('fBool).to[Boolean](testValue) shouldBe true
    root.at('fBool).to[Option[Boolean]](testValue) shouldBe Some(true)
    root.at('fInt).to[Int](testValue) shouldBe 10
    root.at('fLong).to[Long](testValue) shouldBe 100L
    root.at('fDouble).to[Double](testValue) shouldBe 12.3
    root.at('fString).to[String](testValue) shouldBe "hello"
    root.at('fArray).to[List[PValue]](testValue) shouldBe List(PValue(1), PValue(2), PValue(3))
    root.at('fArray).to[List[Int]](testValue) shouldBe List(1, 2, 3)
  }

  it should "focus with dynamics" in {
    root.fBool.to[Boolean](testValue) shouldBe true
    root.fInt.to[Int](testValue) shouldBe 10
    root.fLong.to[Long](testValue) shouldBe 100L
    root.fDouble.to[Double](testValue) shouldBe 12.3
    root.fString.to[String](testValue) shouldBe "hello"
    root.fArray.to[List[Int]](testValue) shouldBe List(1, 2, 3)
  }

  it should "allow map and modify values in" in {
    val expected = testValue.copy(testValue.value + ('fInt -> PValue(root.fInt.to[Int](testValue) * 100)))

    root.fInt.as[Int].modify(_ * 100)(testValue) shouldBe PResult.valid(expected)
    root.fInt.map[Int, Int](_ * 10).as[Int].modify(_ * 10)(testValue) shouldBe PResult.valid(expected)

    val lengthLense = root.fString.map[String, Int](_.length).as[Int]
    lengthLense.get(testValue) shouldBe PResult.valid(5)

    val error = new RuntimeException
    root.fInt.transform((v: Int) => PResult.valid(v * 10)).as[Int].modify(_ * 10)(testValue) shouldBe PResult.valid(expected)
    root.fInt.transform[Int, Int](_ => PResult.invalid(error)).as[Int].modify(_ * 10)(testValue) shouldBe PResult.invalid(error)
    root.fInt.transform[Int, Int](_ => PResult.invalid(error)).asOpt[Int](testValue) shouldBe PResult.valid(None).withWarning(error)
  }

  it should "filter values" in {
    root.fLong.filter[Long](_ > 10).as[Int].get(testValue) shouldBe PResult.valid(100)
    root.fLong.filter[Long](_ < 0).as[Int].get(testValue) shouldBe PResult.invalid(NullValue("Trying decode null value to type Int"))
    root.fLong.filter[Long](_ < 0).asOpt[Int].get(testValue) shouldBe PResult.valid(None)
  }

  it should "direct return value" in {
    root.fString.as[String](testValue) shouldBe PResult.valid("hello")
  }

  "orElse" should "return value from fallback path on primary fail" in {
    root.fStringWrong.as[String].get(testValue) shouldBe a [PError]
    root.fStringWrong.orElse(root.fString).as[String].get(testValue) shouldBe PResult.valid("hello")
  }
}
