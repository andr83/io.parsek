package io.parsek.instances

import java.time._
import java.time.format.DateTimeFormatter

import cats.syntax.either._
import io.parsek.Decoder.Result
import io.parsek.PValue._
import io.parsek.{Decoder, PValue}

import scala.util.Try

/**
  * @author Andrei Tupitcyn
  */
trait DecoderInstances {
  implicit val idDecoder: Decoder[PValue] = Decoder.partial[PValue] {
    case v: PValue => Right(v)
  }

  implicit val nullDecoder: Decoder[Unit] = Decoder.partial[Unit] {
    case Null => Right(())
  }

  implicit val booleanDecoder: Decoder[Boolean] = Decoder.partial[Boolean] {
    case PBoolean(v) => Right(v)
    case PString(str) => Either.catchNonFatal(str.toBoolean)
  }

  implicit val intDecoder: Decoder[Int] = Decoder.partial[Int] {
    case PInt(v) => Right(v)
    case PLong(v) => Right(v.toInt)
    case PDouble(v) => Right(v.toInt)
    case PString(v) => Either.catchNonFatal(v.toInt)
  }

  implicit val longDecoder: Decoder[Long] = Decoder.partial[Long] {
    case PInt(v) => Right(v.toLong)
    case PLong(v) => Right(v)
    case PDouble(v) => Right(v.toLong)
    case PString(v) => Either.catchNonFatal(v.toLong)
    case PInstant(v) => Right(v.toEpochMilli)
  }

  implicit val floatDecoder: Decoder[Float] = Decoder.partial[Float] {
    case PInt(v) => Right(v.toFloat)
    case PLong(v) => Right(v.toFloat)
    case PDouble(v) => Either.catchNonFatal(v.toFloat)
    case PString(v) => Either.catchNonFatal(v.toFloat)
  }

  implicit val doubleDecoder: Decoder[Double] = Decoder.partial[Double] {
    case PInt(v) => Right(v.toDouble)
    case PLong(v) => Right(v.toDouble)
    case PDouble(v) => Right(v)
    case PString(v) => Either.catchNonFatal(v.toDouble)
  }

  implicit val stringDecoder: Decoder[String] = Decoder.partial[String] {
    case PString(v) => Right(v)
    case PInt(v) => Right(v.toString)
    case PLong(v) => Right(v.toString)
    case PDouble(v) => Right(v.toString)
    case PBoolean(v) => Right(v.toString)
  }

  implicit val instantDecoder: Decoder[Instant] = Decoder.partial[Instant] {
    case PInstant(v) => Right(v)
    case PLong(v) => Right(if (v > 100000000000L) Instant.ofEpochMilli(v) else Instant.ofEpochSecond(v))
  }

  implicit val zoneDateTimeDecoder: Decoder[ZonedDateTime] = new Decoder[ZonedDateTime] {
    override def apply(v: PValue): Result[ZonedDateTime] = instantDecoder(v).map(ts => ZonedDateTime.ofInstant(ts, ZoneOffset.UTC))
  }

  implicit val localDateTimeDecoder: Decoder[LocalDateTime] = new Decoder[LocalDateTime] {
    override def apply(v: PValue): Result[LocalDateTime] = instantDecoder(v).map(ts => LocalDateTime.ofInstant(ts, ZoneOffset.UTC))
  }

  implicit val localDateDecoder: Decoder[LocalDate] = new Decoder[LocalDate] {
    override def apply(v: PValue): Result[LocalDate] = instantDecoder(v).map(ts => LocalDateTime.ofInstant(ts, ZoneOffset.UTC).toLocalDate)
  }

  implicit val timestampDecoder: Decoder[java.sql.Timestamp] = Decoder.partial[java.sql.Timestamp] {
    case PInstant(v) => Right(java.sql.Timestamp.from(v))
    case PLong(v) => Right(new java.sql.Timestamp(v))
  }

  private val dateFormatter = DateTimeFormatter.ofPattern("yyyy-mm-dd")
  implicit val sqlDateDecoder: Decoder[java.sql.Date] = Decoder.partial[java.sql.Date] {
    case PInstant(v) => Right(new java.sql.Date(v.toEpochMilli))
    case PLong(v) => Right(new java.sql.Date(v))
    case PString(v) => Either.fromTry(Try(java.sql.Date.valueOf(LocalDate.parse(v, dateFormatter))))
  }

  implicit val vectorDecoder: Decoder[Vector[PValue]] = Decoder.partial[Vector[PValue]] {
    case PArray(v) => Right(v)
  }

  implicit val mapDecoder: Decoder[Map[Symbol, PValue]] = Decoder.partial[Map[Symbol, PValue]] {
    case PMap(v) => Right(v)
  }

  implicit val bytesDecoder: Decoder[Array[Byte]] = Decoder.partial[Array[Byte]] {
    case PBytes(v) => Right(v)
  }

  implicit val pmapDecoder: Decoder[PMap] = Decoder.partial[PMap] {
    case pm: PMap => Right(pm)
  }

  implicit def optDecoder[A : Decoder]: Decoder[Option[A]] = new Decoder[Option[A]] {
    override def apply(v: PValue): Result[Option[A]] = v match {
      case Null => Right(None)
      case _ => implicitly[Decoder[A]].apply(v).map(Some.apply)
    }
  }
}

object DecoderInstances extends DecoderInstances
