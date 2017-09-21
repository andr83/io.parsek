package io.parsek

import scala.reflect.runtime.universe.{TypeTag, typeTag}

/**
 * @author Andrei Tupitcyn
 */
trait Decoder[A] extends Serializable {
  def apply(v: PValue): Decoder.Result[A]
  def unsafe(v: PValue): A = apply(v).fold(throw _, identity)
}

object Decoder {
  type Result[A] = Throwable Either A

  final def partial[A: TypeTag](f: PartialFunction[PValue, Decoder.Result[A]]): Decoder[A] = new Decoder[A] {
    def apply(v: PValue): Decoder.Result[A] =
      f.orElse[PValue, Decoder.Result[A]] {
        case PValue.Null => Left(NullValue(s"Trying decode null value to type ${typeTag[A].tpe}"))
        case other => Left(TypeCastFailure(s"Can not cast value $other to ${typeTag[A].tpe}"))
      }(v)
  }
}
