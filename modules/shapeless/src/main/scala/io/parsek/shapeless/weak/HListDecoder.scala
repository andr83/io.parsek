package io.parsek.shapeless.weak

import io.parsek.Decoder.Result
import io.parsek.PValue.{PMap, PNull}
import io.parsek.{Decoder, Encoder, PValue}
import shapeless.labelled.FieldType
import shapeless.{::, Default, HList, HNil, LabelledGeneric, Lazy, Witness}

trait HListDecoder {

  trait DecoderWithDefaults[T] {
    def apply(v: PValue, defaults: Map[Symbol, PValue]): Result[T]
  }

  implicit def hlistObjectDecoderWithDefaults[K <: Symbol, H, T <: HList](
                                                                           implicit
                                                                           witness: Witness.Aux[K],
                                                                           hDecoder: Lazy[DecoderWithDefaults[H]],
                                                                           tDecoder: Lazy[DecoderWithDefaults[T]]
                                                                         ): DecoderWithDefaults[FieldType[K, H] :: T] =
    new DecoderWithDefaults[FieldType[K, H] :: T] {
      override def apply(v: PValue, defaults: Map[Symbol, PValue]): Result[FieldType[K, H] :: T] = {
        val headResult: Decoder.Result[FieldType[K, H]] = {
          v match {
            case p@PMap(valuesMap) =>
              val fieldName = witness.value
              val newPMap = PMap(valuesMap - fieldName)
              valuesMap
                .get(fieldName)
                .orElse(defaults.get(fieldName))
                .orElse(Some(PNull)) // we try to decode object from PNull, it's possible, for instance, for Option[T]
                .map(pv => hDecoder.value(pv, defaults)
                .right.map(shapeless.labelled.field[K].apply _))
                .getOrElse(Left(new IllegalArgumentException(s"Field $fieldName does not exist in PMap")))
            case _ => Left(new IllegalArgumentException("Case classes have to map to PMap"))
          }
        }

        def buildTail = tDecoder.value.apply(v, defaults)

        for {
          head <- headResult.right
          tail <- buildTail.right
        } yield head :: tail
      }
    }

  implicit def hNilDecoderWithDefaults: DecoderWithDefaults[HNil] = new DecoderWithDefaults[HNil] {
    override def apply(v: PValue, defaults: Map[Symbol, PValue]): Result[HNil] = Right(HNil)
  }

  implicit def decoderToWithDefault[T](implicit d: Decoder[T]): DecoderWithDefaults[T] = new DecoderWithDefaults[T] {
    override def apply(v: PValue, defaults: Map[Symbol, PValue]): Result[T] = d.apply(v)
  }

  implicit def genericDecoder[H, T <: HList, D <: HList](implicit
                                                         gen: LabelledGeneric.Aux[H, T],
                                                         td: Lazy[DecoderWithDefaults[T]],
                                                         defaults: Default.AsRecord.Aux[H, D],
                                                         defaultEncoder: Lazy[Encoder[D]]): Decoder[H] =
    new Decoder[H] {
      def apply(pValue: PValue): Decoder.Result[H] = {
        val defaultPValue: Map[Symbol, PValue] =
          defaultEncoder.value(defaults()) match {
            case PMap(map) => map
            case PNull => Map.empty
            case _ => Map.empty // Strange
          }

        td.value(pValue, defaultPValue).right.map(gen.from)
      }
    }

}

object HListDecoder extends HListDecoder

