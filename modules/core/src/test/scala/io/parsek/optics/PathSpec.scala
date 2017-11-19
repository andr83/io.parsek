package io.parsek.optics

import io.parsek._
import io.parsek.implicits._
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author andr83
  */
class PathSpec extends FlatSpec with Matchers {
  val testValue = pmap(
    'fBool -> PValue(true),
    'fInt -> PValue(10),
    'fLong -> PValue(100L),
    'fDouble -> PValue(12.3),
    'fString -> PValue("hello"),
    'fArray -> PValue(List(1, 2, 3)),
    'fMap -> PValue.pmap('f1 -> PValue(1), 'f2 -> PValue(2), 'f3 -> PValue.pmap('f1 -> PValue(3)), 'f4 -> PValue.pmap('f1 -> PValue(0)))
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

    val expected2 = testValue.copy(testValue.value + ('fInt -> PValue(11)))
    root.fInt.set(11).apply(testValue) shouldBe PResult.valid(expected2)

    val lengthLense = root.fString.map[String, Int](_.length).as[Int]
    lengthLense.get(testValue) shouldBe PResult.valid(5)

    val error = new RuntimeException
    root.fInt.transform[Int, Int](_ => PResult.invalid(error)).as[Int](testValue) shouldBe PResult.invalid(error)
    root.fInt.transform[Int, Int](_ => PResult.invalid(error)).asOpt[Int](testValue) shouldBe PResult.valid(None).withWarning(error)
  }

  it should "filter values" in {
    root.fLong.filter[Long](_ > 10).as[Int].get(testValue) shouldBe PResult.valid(100)
    root.fLong.filter[Long](_ < 0).as[Int].get(testValue) shouldBe PResult.invalid(NullValue("Trying decode null value to type Int"))
    root.fLong.filter[Long](_ < 0).asOpt[Int].get(testValue) shouldBe PResult.valid(None)
    root.fLong.filter[Boolean](identity).asOpt[Boolean].get(testValue) shouldBe PResult.valid(None)
  }

  it should "direct return value" in {
    root.fString.as[String](testValue) shouldBe PResult.valid("hello")
  }

  it should "zoom in multiple values" in {
    root.find[Int] { case (k, v) => k == 'f1 && v > 0 }.getAll(testValue) shouldBe Seq('f1 -> 1, 'f1 -> 3)

    val res = root.find[Int] { case (k, v) => k == 'f1 && v > 0 }.modify { case (k, v) => 'f100 -> v * 100 }(testValue)
    val expected = testValue.copy(testValue.value + ('fMap -> PValue.pmap('f100 -> PValue(100), 'f2 -> PValue(2), 'f3 -> PValue.pmap('f100 -> PValue(300)), 'f4 -> PValue.pmap('f1 -> PValue(0)))))
    res shouldBe PResult.valid(expected)
  }

  it should "set multiple values" in {
    val expected = testValue.copy(testValue.value + ('fMap -> PValue.pmap('f100 -> PValue(100), 'f2 -> PValue(2), 'f3 -> PValue.pmap('f100 -> PValue(100)), 'f4 -> PValue.pmap('f1 -> PValue(0)))))
    root
      .find[Int] { case (k, v) => k == 'f1 && v > 0 }
      .set('f100 -> 100)(testValue) shouldBe PResult.valid(expected)
  }

  "orElse" should "return value from fallback path on primary fail" in {
    root.fStringWrong.as[String].get(testValue) shouldBe a[PError]
    root.fStringWrong.orElse(root.fString).as[String].get(testValue) shouldBe PResult.valid("hello")
  }
}
