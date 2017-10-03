package io.parsek.shapeless.weak

import cats.data.Ior
import io.parsek.Decoder.decode
import io.parsek.{Decoder, PValue}
import io.parsek.PValue._
import org.scalatest.{FunSuite, Matchers, WordSpec}

/**
  * Created by bfattahov on 03/10/2017.
  */
class HListDecoderSpec extends WordSpec with Matchers {

  "Weak HListDecoder" should {
    "break decoding of an Option[T]" in {
      import io.parsek.shapeless.weak.HListDecoder._
      import io.parsek.instances.DecoderInstances._
      decode[Option[Int]](PInt(3)) shouldEqual Right(Some(3))
      decode[Option[Int]](PNull) shouldEqual Right(None)
      decode[Option[Int]](PString("not int")) should be(a[Left[Throwable, _]])
    }

    "decode case classes" in {
      import io.parsek.shapeless.weak.HListDecoder._
      import io.parsek.shapeless.HListEncoder._
      import io.parsek.instances.DecoderInstances._
      import io.parsek.instances.EncoderInstances._
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

      val pvalue: PValue = PMap(Map(
        'name -> PString("Eugene"),
        'surname -> PString("Lukashin"),
        'address -> PMap(
          Map('flat -> PInt(25),
            'building -> PInt(13),
            'city -> PString("Leningrad"),
            'street -> PString("3rd Builders Street"))
        )
      ))

      decode[User](pvalue) shouldEqual Right(jenya)

      case class Small(value: Int)
      decode[Small](PValue.pmap('value -> PInt(3))) shouldEqual Right(Small(3))

      case class SmallOpt(value: Option[Int])
      decode[SmallOpt](PValue.pmap('value -> PInt(3))) shouldEqual Right(SmallOpt(Some(3)))
    }

    "decode case classes with defaults" in {
      import HListDecoder._
      import io.parsek.instances.DecoderInstances._
      import io.parsek.shapeless.HListEncoder._
      import io.parsek.instances.EncoderInstances._

      case class A(name: String, b: Int = 4)

      decode[A](PValue.pmap('name -> PString("aname"), 'b -> PInt(5))) shouldEqual Right(A("aname", 5))
      decode[A](PValue.pmap('name -> PString("aname"))) shouldEqual Right(A("aname", b = 4))
      decode[A](PValue.pmap('name344 -> PString("aname"))) should be (a[Left[_, _]])
    }

    "decode case classes with Options" in {
      import HListDecoder._
      import io.parsek.instances.DecoderInstances._
      import io.parsek.shapeless.HListEncoder._
      import io.parsek.instances.EncoderInstances._
      case class B(name: String, b: Option[Int] = None)
      decode[B](PValue.pmap('name -> PString("aname"), 'b -> PInt(5))) shouldEqual Right(B("aname", Some(5)))
      decode[B](PValue.pmap('name -> PString("aname"))) shouldEqual Right(B("aname", None))
    }

    "Work with non-matching PValues" in {
      import HListDecoder._
      import io.parsek.instances.DecoderInstances._
      import io.parsek.shapeless.HListEncoder._
      import io.parsek.instances.EncoderInstances._

      case class C(f1: String)
      decode[C](PValue.pmap('f1-> PString("f1"), 'unexpected -> PInt(5))) should be (Right(C("f1")))

      case class D(a:String, b:Option[Int])

      decode[D](PValue.pmap('a-> PString("a"))) should be (Right(D("a", None)))
      decode[D](PValue.pmap('a-> PString("a"), 'b -> PInt(45))) should be (Right(D("a", Some(45))))
    }

  }

}
