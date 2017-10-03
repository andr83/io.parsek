package io.parsek.shapeless

import io.parsek.{Encoder, PValue}
import io.parsek.PValue._
import org.scalatest.{FlatSpec, Matchers, WordSpec}

/**
  * Created by bfattahov on 02/10/2017.
  */
class HListEncoderSpec extends WordSpec with Matchers {

  "HListEncoder" can {
    "encode case classes" in {
      case class Address(street: String = "3rd Builders Street", city: String = "DefaultCity", building: Int = 13, flat: Int = 25)
      //recently it does not work with recursive types
      case class User(surname: String = "Lukashin", name: String = "Eugene", address: Address = Address() /*, spouse: Option[User] = None*/)

      val jenya = User()

      import io.parsek.instances.EncoderInstances._
      import HListEncoder._

      Encoder.encode(jenya) shouldEqual {
        PMap(Map(
          'name -> PString("Eugene"),
          'surname -> PString("Lukashin"),
          'address -> PMap(
            Map('flat -> PInt(25),
              'building -> PInt(13),
              'city -> PString("DefaultCity"),
              'street -> PString("3rd Builders Street"))
          )
        ))
      }
    }


    "properly work with Option" in {

      import io.parsek.instances.EncoderInstances._
      import HListEncoder._

      Encoder.encode(Some(2)) shouldEqual PInt(2)
      Encoder.encode(Option(3)) shouldEqual PInt(3)
      Encoder.encode(None.asInstanceOf[Option[Int]]) shouldEqual PNull
      Encoder.encode(None) shouldEqual PNull
    }

    "work with case classes with Option[T]" in {
      import io.parsek.instances.EncoderInstances._
      import HListEncoder._
      case class CC(field: Option[Int])

      Encoder.encode(CC(Some(2))) shouldEqual PValue.pmap('field -> PInt(2))
    }

  }
}
