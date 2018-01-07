package io.parsek.shapeless

import io.parsek.PValue
import io.parsek.PValue._
import io.parsek._
import io.parsek.PResult.{valid, invalid}
import io.parsek.implicits._
import io.parsek.shapeless.implicits._
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by bfattahov on 03/10/2017.
  */
class HListDecoderSpec extends WordSpec with Matchers {

  "HListDecoder" should {

    "break decoding of an Option[T]" in {
      import io.parsek.instances.DecoderInstances._
      PInt(3).as[Option[Int]] shouldEqual valid(Some(3))
      PNull.as[Option[Int]] shouldEqual valid(None)
      PString("not int").as[Option[Int]] should be(a[PError])
    }

    "decode case classes" in {
      case class Address(street: String, city: String, building: Int, flat: Int)
      //recently it does not work with recursive types
      case class User(surname: String, name: String, address: Address /*, spouse: Option[User] = None*/)

      val jenya = User(
        surname = "Lukashin",
        name = "Eugene",
        address = Address(
          street = "3rd Builders Street",
          city = "Leningrad",
          building = 13,
          flat = 25)
      )

      val pvalue: PValue = PValue.pmap(
        'name -> PString("Eugene"),
        'surname -> PString("Lukashin"),
        'address -> PValue.pmap(
          'flat -> PInt(25),
          'building -> PInt(13),
          'city -> PString("Leningrad"),
          'street -> PString("3rd Builders Street")
        )
      )

      pvalue.asUnsafe[User] shouldEqual jenya

      case class Small(value: Int)
      PValue.pmap('value -> PInt(3)).asUnsafe[Small] shouldEqual Small(3)

      case class SmallOpt(value: Option[Int])
      PValue.pmap('value -> PInt(3)).asUnsafe[SmallOpt] shouldEqual SmallOpt(Some(3))
    }

    "decode case classes with defaults" in {
      case class A(name: String, b: Int = 4)

      PValue.pmap('name -> PString("aname"), 'b -> PInt(5)).asUnsafe[A] shouldEqual A("aname", 5)
      PValue.pmap('name -> PString("aname")).asUnsafe[A] shouldEqual A("aname")
      PValue.pmap('name344 -> PString("aname")).as[A] should be(a[PError])
    }

    "decode case classes with Options" in {
      case class B(name: String, b: Option[Int] = None)
      PValue.pmap('name -> PString("aname"), 'b -> PInt(5)).asUnsafe[B] shouldEqual B("aname", Some(5))
      PValue.pmap('name -> PString("aname")).as[B] shouldEqual valid(B("aname", None))
    }
  }

  "HListDecoder" should {

    "fail in non-matching PValues in strict Configuration" in {
      import Configuration.Strict._

      case class C(f1: String)
      PValue.pmap('f1 -> PString("f1"), 'unexpected -> PInt(5)).as[C] should be(a[PError])

      case class D(a: String, b: Option[Int])

      PValue.pmap('a -> PString("a")).as[D] should be(a[PError])
      PValue.pmap('a -> PString("a"), 'b -> PInt(45)).asUnsafe[D] should be(D("a", Some(45)))
    }

    "work with non-matching PValues in weak Configuration" in {
      case class C(f1: String)
      PValue.pmap('f1 -> PString("f1"), 'unexpected -> PInt(5)).asUnsafe[C] should be(C("f1"))

      case class D(a: String, b: Option[Int])

      PValue.pmap('a -> PString("a")).asUnsafe[D] should be(D("a", None))
      PValue.pmap('a -> PString("a"), 'b -> PInt(45)).asUnsafe[D] should be(D("a", Some(45)))
    }
  }

}
