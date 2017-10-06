package io.parsek.shapeless.instances

import io.parsek.PValue.{PMap, PNull}
import io.parsek.{Encoder, PValue}
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}

trait HListEncoder extends HListEncoderLowPriority {

  implicit def optionEncoder[T](implicit enc: Encoder[T]): Encoder[Option[T]] = new Encoder[Option[T]] {
    override def apply(a: Option[T]): PValue = a match {
      case Some(x) => enc(x)
      case None => PNull
    }
  }
}

trait HListEncoderLowPriority {

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
                                                               implicit
                                                               witness: Witness.Aux[K],
                                                               hEncoder: Lazy[Encoder[H]],
                                                               tEncoder: Lazy[Encoder[T]]
                                                             ): Encoder[FieldType[K, H] :: T] =
    new Encoder[FieldType[K, H] :: T] {
      override def apply(v: FieldType[K, H] :: T): PValue = {
        val tuple = witness.value -> hEncoder.value.apply(v.head)
        tEncoder.value.apply(v.tail) match {
          case PMap(map) => PMap(map + tuple)
          case PNull => PMap(Map(tuple))
          case _ => PMap(Map(tuple)) // could not be there
        }
      }
    }

  implicit def hNilEncoder: Encoder[HNil] = new Encoder[HNil] {
    override def apply(a: HNil): PValue = PNull
  }

  implicit def genericEncoder[H, T <: HList](implicit
                                             gen: LabelledGeneric.Aux[H, T],
                                             te: Lazy[Encoder[T]]): Encoder[H] = new Encoder[H] {
    override def apply(h: H): PValue = te.value(gen.to(h))
  }


}

object HListEncoder extends HListEncoder