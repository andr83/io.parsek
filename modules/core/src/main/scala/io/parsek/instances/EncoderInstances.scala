package io.parsek.instances

import java.time.{Instant, LocalDate}

import io.parsek.{Encoder, PValue}

import scala.language.higherKinds

/**
  * @author Andrei Tupitcyn
  */
trait EncoderInstances0 {
  implicit def traversableEncoder[A, C[A] <: Iterable[A]](implicit e: Encoder[A]): Encoder[C[A]] = Encoder.pure[C[A]](it => {
    PValue.fromValues(it.map(e.apply))
  })

  implicit def optionEncoder[T](implicit enc: Encoder[T]): Encoder[Option[T]] = new Encoder[Option[T]] {
    override def apply(a: Option[T]): PValue = a match {
      case Some(x) => enc(x)
      case None => PValue.PNull
    }
  }

  implicit def mapKeySymbolEncoder[A](implicit e: Encoder[A]): Encoder[Map[Symbol, A]] = Encoder.pure[Map[Symbol, A]](m => {
    PValue.fromMap(m.mapValues(e.apply))
  })

  implicit def mapKeyStringEncoder[A](implicit e: Encoder[A]): Encoder[Map[String, A]] = Encoder.pure[Map[String, A]](m => {
    PValue.fromMap(m.map { case (k, v) => Symbol(k) -> e(v) })
  })
}

trait EncoderInstances extends EncoderInstances0 {
  implicit val idEncoder: Encoder[PValue] = Encoder.pure[PValue](identity)
  implicit val booleanEncoder: Encoder[Boolean] = Encoder.pure[Boolean](PValue.fromBoolean)
  implicit val intEncoder: Encoder[Int] = Encoder.pure[Int](PValue.fromInt)
  implicit val longEncoder: Encoder[Long] = Encoder.pure[Long](PValue.fromLong)
  implicit val doubleEncoder: Encoder[Double] = Encoder.pure[Double](PValue.fromDouble)
  implicit val stringEncoder: Encoder[String] = Encoder.pure[String](PValue.fromString)
  implicit val instantEncoder: Encoder[Instant] = Encoder.pure[Instant](PValue.fromInstant)
  implicit val localDateEncoder: Encoder[LocalDate] = Encoder.pure[LocalDate](PValue.fromLocalDate)
}

object EncoderInstances extends EncoderInstances