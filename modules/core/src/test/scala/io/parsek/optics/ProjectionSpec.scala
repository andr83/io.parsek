//package io.parsek.optics
//
//import io.parsek._
//import io.parsek.implicits._
//import org.scalatest.{FlatSpec, Matchers}
//
///**
//  * @author Andrei Tupitcyn
//  */
//class ProjectionSpec extends FlatSpec with Matchers {
//  "Projection" should "transform PMap value to another PMap" in {
//    val p = Projection(
//      'newField -> root.field1.as,
//      'innerField -> Projection(
//        'inner1 -> root.field2.as[String],
//        'inner2 -> root.field3.as[Double]
//      )
//    )
//    val res = p.transform(pmap(
//      'field1 -> PValue(21),
//      'field2 -> PValue("World"),
//      'field3 -> PValue(16.2)
//    ))
//    res shouldBe PResult.valid(pmap(
//      'newField -> PValue(21),
//      'innerField -> pmap(
//        'inner1 -> PValue("World"),
//        'inner2 -> PValue(16.2)
//      )
//    ))
//  }
//
//  it should "skip opt values" in {
//    val p = Projection(
//      'field1 -> root.field1.as[String],
//      'field2 -> root.field2.filter((v:Boolean)=>v).asOpt[Boolean]
//    )
//
//    val res = p.transform(pmap(
//      'field1 -> PValue("hello"),
//      'field2 -> PValue(false)
//    ))
//
//    res shouldBe PResult.valid(pmap(
//      'field1 -> PValue("hello")
//    ))
//  }
//}
