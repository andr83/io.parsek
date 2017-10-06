package io.parsek.shapeless

import io.parsek.Encoder
import io.parsek.PValue._
import io.parsek.implicits._
import io.parsek.shapeless.implicits._
import org.scalatest.{Matchers, WordSpec}

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

      Encoder.encode(jenya) shouldEqual pmap(
          'name -> PString("Eugene"),
          'surname -> PString("Lukashin"),
          'address -> pmap(
            'flat -> PInt(25),
            'building -> PInt(13),
            'city -> PString("DefaultCity"),
            'street -> PString("3rd Builders Street")
          )
        )
    }


    "properly work with Option" in {

      Some(2).toPValue shouldEqual PInt(2)
      Option(3).toPValue shouldEqual PInt(3)
      None.asInstanceOf[Option[Int]].toPValue shouldEqual PNull
      Encoder.encode(None) shouldEqual PNull
    }

    "work with case classes with Option[T]" in {
      case class CC(field: Option[Int])

      Encoder.encode(CC(Some(2))) shouldEqual pmap('field -> PInt(2))
    }

  }
}
