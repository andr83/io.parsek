package io.parsek

import scala.reflect.runtime.universe.{TypeTag, typeTag}

/**
  * Convert `PValue` to `A` type.
  *
  * @author Andrei Tupitcyn
  */
trait Decoder[A] extends Serializable {
  def apply(v: PValue): PResult[A]

  def unsafe(v: PValue): A = apply(v).fold(nel => throw nel.head, identity)
}

object Decoder {
  final def partial[A: TypeTag](f: PartialFunction[PValue, PResult[A]]): Decoder[A] = new Decoder[A] {
    def apply(v: PValue): PResult[A] =
      f.orElse[PValue, PResult[A]] {
        case PValue.Null => PResult.invalid(NullValue(s"Trying decode null value to type ${typeTag[A].tpe}"))
        case other => PResult.invalid(TypeCastFailure(s"Can not cast value $other to ${typeTag[A].tpe}"))
      }(v)
  }

  def decode[T](pv: PValue)(implicit dec: Decoder[T]): PResult[T] = dec(pv)
}
