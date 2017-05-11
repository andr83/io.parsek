package io.parsek

import java.time.Instant

import scala.language.higherKinds

/**
  * @author Andrei Tupitcyn
  */
trait DefaultEncoders {
  implicit val idEncoder = Encoder.pure[PValue](identity)
  implicit val booleanEncoder = Encoder.pure[Boolean](PValue.fromBoolean)
  implicit val intEncoder = Encoder.pure[Int](PValue.fromInt)
  implicit val longEncoder = Encoder.pure[Long](PValue.fromLong)
  implicit val doubleEncoder = Encoder.pure[Double](PValue.fromDouble)
  implicit val stringEncoder = Encoder.pure[String](PValue.fromString)
  implicit val instantEncoder = Encoder.pure[Instant](PValue.fromInstant)
  implicit def traversableEncoder[A, C[A] <: Iterable[A]](implicit e: Encoder[A]) = Encoder.pure[C[A]](it => {
    PValue.fromValues(it.map(e.apply))
  })
  implicit def mapEncoder[A](implicit e: Encoder[A]) = Encoder.pure[Map[Symbol, A]](m=> {
    PValue.fromMap(m.mapValues(e.apply))
  })
}

object DefaultEncoders extends DefaultDecoders