package io.parsek.shapeless.instances

import _root_.shapeless.labelled.FieldType
import _root_.shapeless.{::, Default, HList, HNil, LabelledGeneric, Lazy, Witness, labelled}
import io.parsek.PResult.{invalid, valid}
import io.parsek.PValue.{PMap, PNull}
import io.parsek._
import io.parsek.shapeless.Configuration

trait HListDecoder {
  trait DecoderWithDefaults[T] {
    def apply(v: PValue, defaults: Map[Symbol, PValue]): PResult[T]
  }

  implicit def hnilDecoderWithDefaults(implicit decodeConfiguration: Configuration): DecoderWithDefaults[HNil] = new DecoderWithDefaults[HNil] {
    override def apply(v: PValue, defaults: Map[Symbol, PValue]): PResult[HNil] =
      if (decodeConfiguration.allowAdditionalFields)
        valid(HNil)
      else
        v match {
          case PNull => valid(HNil)
          case PMap(map) if map.isEmpty => valid(HNil)
          case x => invalid(new IllegalArgumentException(
            s"Pvalue is not fully converted to case class. " +
              s"Check your data or use io.parsek.shapeless.weak.HListDecoder to avoid this error. " +
              s"Rest part is $x"))
        }
  }

  implicit def hlistProductDecoderWithDefaults[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hDecoder: Lazy[DecoderWithDefaults[H]],
    tDecoder: Lazy[DecoderWithDefaults[T]],
    decodeConfiguration: Configuration
  ): DecoderWithDefaults[FieldType[K, H] :: T] =
    new DecoderWithDefaults[FieldType[K, H] :: T] {
      override def apply(v: PValue, defaults: Map[Symbol, PValue]): PResult[FieldType[K, H] :: T] = {
        val headResult: PResult[(PValue, FieldType[K, H])] = {
          v match {
            case p@PMap(valuesMap) =>
              val fieldName = decodeConfiguration.lookUpFieldName(witness.value)
              val newPMap = PMap(valuesMap - fieldName)
              valuesMap
                .get(fieldName)
                .orElse(if (decodeConfiguration.useDefaults) defaults.get(fieldName) else None)
                .orElse(if (decodeConfiguration.tryPNullForEmptyFields) Some(PNull) else None)
                .map(pv => hDecoder.value(pv, defaults)
                  .map(v => newPMap -> labelled.field[K](v))
                  .errorMap(errors => errors.map {
                    case _: NullValue => NullField(fieldName, s"Field $fieldName does not exist in PMap")
                    case other => other
                  })
                )
                .getOrElse(invalid(NullField(fieldName, s"Field $fieldName does not exist in PMap")))
            case _ => invalid(new IllegalArgumentException("Case classes have to map to PMap"))
          }
        }

        def buildTail(newPValue: PValue) = tDecoder.value.apply(newPValue, defaults)

        for (
          head <- headResult;
          tail <- buildTail(head._1)
        ) yield head._2 :: tail
      }
    }

  implicit def decoderToWithDefault[T](implicit d: Decoder[T]): DecoderWithDefaults[T] = new DecoderWithDefaults[T] {
    override def apply(v: PValue, defaults: Map[Symbol, PValue]): PResult[T] = d.apply(v)
  }

  implicit def genericDecoder[H, T <: HList, D <: HList](implicit
                                                         gen: LabelledGeneric.Aux[H, T],
                                                         td: Lazy[DecoderWithDefaults[T]],
                                                         defaults: Default.AsRecord.Aux[H, D],
                                                         defaultEncoder: Lazy[Encoder[D]],
                                                         decodeConfiguration: Configuration): Decoder[H] =
    new Decoder[H] {
      def apply(pValue: PValue): PResult[H] = {
        val defaultPValue: Map[Symbol, PValue] =
          defaultEncoder.value(defaults()) match {
            case PMap(map) => map
            case PNull => Map.empty
            case _ => Map.empty // Strange
          }
        td.value(pValue, defaultPValue).map(gen.from)
      }
    }
}

object HListDecoder extends HListDecoder

