package io.parsek

import io.parsek.PPath.root
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Andrei Tupitcyn
  */
class ProjectionSpec extends FlatSpec with Matchers {
  "Projection" should "transform PMap value to another PMap" in {
    val p = Projection(
      'newField -> root.field1.value,
      'innerField -> Projection(
        'inner1 -> root.field2.value,
        'inner2 -> root.field3.value
      )
    )
    val res = p(pmap(
      'field1 -> PValue(21),
      'field2 -> PValue("World"),
      'field3 -> PValue(16.2)
    ))
    res shouldBe Right(pmap(
      'newField -> PValue(21),
      'innerField -> pmap(
        'inner1 -> PValue("World"),
        'inner2 -> PValue(16.2)
      )
    ))
  }
}
