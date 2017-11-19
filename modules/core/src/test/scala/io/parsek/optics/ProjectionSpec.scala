package io.parsek.optics

import io.parsek._
import io.parsek.implicits._
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Andrei Tupitcyn
  */
class ProjectionSpec extends FlatSpec with Matchers {
  "Projection" should "transform PMap value to another PMap" in {
    val p = Projection(
      'newField -> root.field1.as[PValue],
      'innerField -> Projection(
        'inner1 -> root.field2.as[String],
        'inner2 -> root.field3.as[Double]
      )
    )

    val res = p.get(pmap(
      'field1 -> PValue(21),
      'field2 -> PValue("World"),
      'field3 -> PValue(16.2)
    ))
    res shouldBe PResult.valid(pmap(
      'newField -> PValue(21),
      'innerField -> pmap(
        'inner1 -> PValue("World"),
        'inner2 -> PValue(16.2)
      )
    ))
  }

  it should "skip opt values" in {
    val p = Projection(
      'field1 -> root.field1.as[String],
      'field2 -> root.field2.filter((v: Boolean) => v).asOpt[Boolean]
    )

    val res = p.get(pmap(
      'field1 -> PValue("hello"),
      'field2 -> PValue(false)
    ))

    res shouldBe PResult.valid(pmap(
      'field1 -> PValue("hello")
    ))
  }

  it should "find and map values in" in {
    val p = Projection(
      'field1 -> root.field1
        .find[Int] { case (k, _) => k == 'f1 }
        .map { case (k, v) => k -> (v * 100) }
        .as[PValue],
      'field2 -> root.field2
        .filter((v: Boolean) => v)
        .asOpt[Boolean]
    )

    val res = p.get(pmap(
      'field1 -> PValue.pmap('f1 -> PValue(1), 'f2 -> PValue(2), 'f3 -> PValue.pmap('f1 -> PValue(3)), 'f4 -> PValue.pmap('f1 -> PValue(0))),
      'field2 -> PValue("hello")
    ))

    res shouldBe PResult.valid(pmap(
      'field1 -> PValue.pmap('f1 -> PValue(100), 'f2 -> PValue(2), 'f3 -> PValue.pmap('f1 -> PValue(300)), 'f4 -> PValue.pmap('f1 -> PValue(0)))
    ))
  }
}
