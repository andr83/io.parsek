package io.parsek.shapeless

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
                                                                           tDecoder: Lazy[DecoderWithDefaults[T]],
                                                                           decodeConfiguration: Configuration
                                                                         ): DecoderWithDefaults[FieldType[K, H] :: T] =
    new DecoderWithDefaults[FieldType[K, H] :: T] {
      override def apply(v: PValue, defaults: Map[Symbol, PValue]): Result[FieldType[K, H] :: T] = {
        val headResult: Decoder.Result[(PValue, FieldType[K, H])] = {
          v match {
            case p@PMap(valuesMap) =>
              val fieldName = decodeConfiguration.lookUpFieldName(witness.value)
              val newPMap = PMap(valuesMap - fieldName)
              valuesMap
                .get(fieldName)
                .orElse(if (decodeConfiguration.useDefaults) defaults.get(fieldName) else None)
                .orElse(if (decodeConfiguration.tryPNullForEmptyFields) Some(PNull) else None)
                .map(pv => hDecoder.value(pv, defaults)
                  .right.map(shapeless.labelled.field[K].apply _)
                  .right.map(newPMap -> _))
                .getOrElse(Left(new IllegalArgumentException(s"Field $fieldName does not exist in PMap")))
            case _ => Left(new IllegalArgumentException("Case classes have to map to PMap"))
          }
        }

        def buildTail(newPValue: PValue) = tDecoder.value.apply(newPValue, defaults)

        for (
          head <- headResult.right;
          tail <- buildTail(head._1).right
        ) yield head._2 :: tail
      }
    }

  implicit def hNilDecoderWithDefaults(implicit decodeConfiguration: Configuration): DecoderWithDefaults[HNil] = new DecoderWithDefaults[HNil] {
    override def apply(v: PValue, defaults: Map[Symbol, PValue]): Result[HNil] =
      if (decodeConfiguration.allowAdditionalFields)
        Right(HNil)
      else
        v match {
          case PNull => Right(HNil)
          case PMap(map) if map.isEmpty => Right(HNil)
          case x => Left(new IllegalArgumentException(
            s"Pvalue is not fully converted to case class. " +
              s"Check your data or use io.parsek.shapeless.weak.HListDecoder to avoid this error. " +
              s"Rest part is $x"))
        }
  }

  implicit def decoderToWithDefault[T](implicit d: Decoder[T]): DecoderWithDefaults[T] = new DecoderWithDefaults[T] {
    override def apply(v: PValue, defaults: Map[Symbol, PValue]): Result[T] = d.apply(v)
  }

  implicit def genericDecoder[H, T <: HList, D <: HList](implicit
                                                         gen: LabelledGeneric.Aux[H, T],
                                                         td: Lazy[DecoderWithDefaults[T]],
                                                         defaults: Default.AsRecord.Aux[H, D],
                                                         defaultEncoder: Lazy[Encoder[D]],
                                                         decodeConfiguration: Configuration): Decoder[H] =
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

object HListDecoder extends HListDecoder with Configuration.Weak

