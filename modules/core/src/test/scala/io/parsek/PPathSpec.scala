package io.parsek

import org.scalatest.{FlatSpec, Matchers}
import io.parsek.PPath.root

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
    root.at('fBool).boolean.getOption(testValue) shouldBe Some(true)
    root.at('fInt).int.getOption(testValue) shouldBe Some(10)
    root.at('fLong).long.getOption(testValue) shouldBe Some(100L)
    root.at('fDouble).double.getOption(testValue) shouldBe Some(12.3)
    root.at('fString).string.getOption(testValue) shouldBe Some("hello")
    root.at('fArray).arr.getOption(testValue) shouldBe Some(Vector(PValue(1), PValue(2), PValue(3)))
  }

  it should "focus with dynamics" in {
    root.fBool.boolean.getOption(testValue) shouldBe Some(true)
    root.fInt.int.getOption(testValue) shouldBe Some(10)
    root.fLong.long.getOption(testValue) shouldBe Some(100L)
    root.fDouble.double.getOption(testValue) shouldBe Some(12.3)
    root.fString.string.getOption(testValue) shouldBe Some("hello")
    root.fArray.arr.getOption(testValue) shouldBe Some(Vector(PValue(1), PValue(2), PValue(3)))
  }

  it should "allow map and modify values in" in {
    val expected = testValue.copy(testValue.value + ('fInt -> PValue(root.fInt.int.getOption(testValue).getOrElse(0) * 100)))

    root.fInt.int.modify(_ * 100)(testValue) shouldBe expected
    root.fInt.map[Int, Int](_ * 10).int.modify(_ * 10)(testValue) shouldBe expected

    val lengthLense = root.fString.map[String, Int](_.length).int
    lengthLense.getOption(testValue) shouldBe Some(5)

    root.fInt.mapT[Int, Int](v => Right(v * 10)).int.modify(_ * 10)(testValue) shouldBe expected
    root.fInt.mapT[Int, Int](_ => Left(new RuntimeException)).int.modify(_ * 10)(testValue) shouldBe testValue
  }

  it should "filter values" in {
    root.fLong.filter[Long](_ > 10).int.getOption(testValue) shouldBe Some(100)
    root.fLong.filter[Long](_ < 0).int.get(testValue) shouldBe Left(FilterFailure)
  }

  "orElse" should "return value from fallback path on primary fail" in {
    root.fStringWrong.string.get(testValue) shouldBe a [Left[_, _]]
    root.fStringWrong.orElse(root.fString).string.getOption(testValue) shouldBe Some("hello")
  }

  "memoize" should "cache result for the source value" in {
    val lens = root.fArray.map[Vector[PValue], Vector[PValue]](arr => arr :+ PValue(4))
    val curr = lens.memoize

    val v0 = lens.arr.getOption(testValue).get
    val v1 = curr.arr.getOption(testValue).get
    val v2 = curr.arr.getOption(testValue).get
    val v3 = curr.arr.getOption(PValue(testValue.value + ('fBool -> PValue(false)))).get

    v0 shouldNot be theSameInstanceAs v1
    v1 should be theSameInstanceAs v2
    v2 shouldNot be theSameInstanceAs v3
  }
}
