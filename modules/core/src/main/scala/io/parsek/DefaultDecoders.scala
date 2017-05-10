package io.parsek

import java.time.Instant

import cats.syntax.either._
import io.parsek.PValue._

/**
  * @author Andrei Tupitcyn
  */
trait DefaultDecoders {
  implicit val idDecoder = Decoder.partial[PValue] {
    case v: PValue=> Right(v)
  }

  implicit val nullDecoder = Decoder.partial[Unit] {
    case Null=> Right(())
  }

  implicit val booleanDecoder = Decoder.partial[Boolean] {
    case PBoolean(v) => Right(v)
    case PString(str) => Either.catchNonFatal(str.toBoolean)
  }

  implicit val intDecoder = Decoder.partial[Int] {
    case PInt(v) => Right(v)
    case PLong(v) => Right(v.toInt)
    case PDouble(v) => Right(v.toInt)
    case PString(v)=> Either.catchNonFatal(v.toInt)
  }

  implicit val longDecoder = Decoder.partial[Long] {
    case PInt(v) => Right(v.toLong)
    case PLong(v) => Right(v)
    case PDouble(v) => Right(v.toLong)
    case PString(v)=> Either.catchNonFatal(v.toLong)
    case PTime(v) => Right(v.toEpochMilli)
  }

  implicit val doubleDecoder = Decoder.partial[Double] {
    case PInt(v) => Right(v.toDouble)
    case PLong(v) => Right(v.toDouble)
    case PDouble(v) => Right(v)
    case PString(v)=> Either.catchNonFatal(v.toDouble)
  }

  implicit val stringDecoder = Decoder.partial[String] {
    case PString(v) => Right(v)
    case PInt(v) => Right(v.toString)
    case PLong(v) => Right(v.toString)
    case PDouble(v) => Right(v.toString)
    case PBoolean(v) => Right(v.toString)
  }

  implicit val instantDecoder = Decoder.partial[Instant] {
    case PTime(v) => Right(v)
    case PLong(v) => Right(Instant.ofEpochMilli(v))
  }

  implicit val vectorDecoder = Decoder.partial[Vector[PValue]] {
    case PArray(v) => Right(v)
  }

  implicit val mapDecoder = Decoder.partial[Map[Symbol, PValue]] {
    case PMap(v) => Right(v)
  }

  implicit val bytesDecoder = Decoder.partial[Array[Byte]] {
    case PBytes(v) => Right(v)
  }
}

object DefaultDecoders extends DefaultDecoders
