package io.parsek.instances

import java.time._
import java.time.format.DateTimeFormatter

import io.parsek.PResult._
import io.parsek.PValue._
import io.parsek.{Decoder, PResult, PValue}
import io.parsek.syntax.traversable._

import scala.reflect.runtime.universe.TypeTag
import scala.collection.generic.CanBuildFrom

/**
  * @author Andrei Tupitcyn
  */
trait DecoderInstances {
  implicit val idDecoder: Decoder[PValue] = Decoder.partial[PValue] {
    case v: PValue => valid(v)
  }

  implicit val nullDecoder: Decoder[Unit] = Decoder.partial[Unit] {
    case Null => valid(())
  }

  implicit val booleanDecoder: Decoder[Boolean] = Decoder.partial[Boolean] {
    case PBoolean(v) => valid(v)
    case PString(str) => catchNonFatal(str.toBoolean)
  }

  implicit val intDecoder: Decoder[Int] = Decoder.partial[Int] {
    case PInt(v) => valid(v)
    case PLong(v) => valid(v.toInt)
    case PDouble(v) => valid(v.toInt)
    case PString(v) => catchNonFatal(v.toInt)
  }

  implicit val longDecoder: Decoder[Long] = Decoder.partial[Long] {
    case PInt(v) => valid(v.toLong)
    case PLong(v) => valid(v)
    case PDouble(v) => valid(v.toLong)
    case PString(v) => catchNonFatal(v.toLong)
    case PInstant(v) => valid(v.toEpochMilli)
  }

  implicit val floatDecoder: Decoder[Float] = Decoder.partial[Float] {
    case PInt(v) => valid(v.toFloat)
    case PLong(v) => valid(v.toFloat)
    case PDouble(v) => catchNonFatal(v.toFloat)
    case PString(v) => catchNonFatal(v.toFloat)
  }

  implicit val doubleDecoder: Decoder[Double] = Decoder.partial[Double] {
    case PInt(v) => valid(v.toDouble)
    case PLong(v) => valid(v.toDouble)
    case PDouble(v) => valid(v)
    case PString(v) => catchNonFatal(v.toDouble)
  }

  implicit val stringDecoder: Decoder[String] = Decoder.partial[String] {
    case PString(v) => valid(v)
    case PInt(v) => valid(v.toString)
    case PLong(v) => valid(v.toString)
    case PDouble(v) => valid(v.toString)
    case PBoolean(v) => valid(v.toString)
    case PInstant(v) => valid(DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(LocalDateTime.ofInstant(v, ZoneId.systemDefault())))
    case PDate(v) => valid(DateTimeFormatter.ISO_LOCAL_DATE.format(v))
  }

  implicit val instantDecoder: Decoder[Instant] = Decoder.partial[Instant] {
    case PInstant(v) => valid(v)
    case PLong(v) => valid(if (v > 100000000000L) Instant.ofEpochMilli(v) else Instant.ofEpochSecond(v))
  }

  implicit val zoneDateTimeDecoder: Decoder[ZonedDateTime] = new Decoder[ZonedDateTime] {
    override def apply(v: PValue): PResult[ZonedDateTime] = instantDecoder(v).map(ts => ZonedDateTime.ofInstant(ts, ZoneOffset.UTC))
  }

  implicit val localDateTimeDecoder: Decoder[LocalDateTime] = new Decoder[LocalDateTime] {
    override def apply(v: PValue): PResult[LocalDateTime] = instantDecoder(v).map(ts => LocalDateTime.ofInstant(ts, ZoneOffset.UTC))
  }

  implicit val localDateDecoder: Decoder[LocalDate] = Decoder.partial[LocalDate] {
    case PDate(v) => valid(v)
    case PInstant(v) => valid(LocalDateTime.ofInstant(v, ZoneId.systemDefault()).toLocalDate)
  }

  implicit val timestampDecoder: Decoder[java.sql.Timestamp] = Decoder.partial[java.sql.Timestamp] {
    case PInstant(v) => valid(java.sql.Timestamp.from(v))
    case PLong(v) => valid(new java.sql.Timestamp(v))
  }

  private val dateFormatter = DateTimeFormatter.ofPattern("yyyy-mm-dd")

  implicit val sqlDateDecoder: Decoder[java.sql.Date] = Decoder.partial[java.sql.Date] {
    case PDate(v) => valid(java.sql.Date.valueOf(v))
    case PInstant(v) => valid(new java.sql.Date(v.toEpochMilli))
    case PLong(v) => valid(new java.sql.Date(v))
    case PString(v) => catchNonFatal(java.sql.Date.valueOf(LocalDate.parse(v, dateFormatter)))
  }

  implicit val vectorDecoder: Decoder[Vector[PValue]] = Decoder.partial[Vector[PValue]] {
    case PArray(v) => valid(v)
  }

  implicit val mapKeySymbolPValueDecoder: Decoder[Map[Symbol, PValue]] = Decoder.partial[Map[Symbol, PValue]] {
    case PMap(v) => valid(v)
  }

  implicit def mapKeySymbolDecoder[A: TypeTag](implicit decoderA: Decoder[A]): Decoder[Map[Symbol, A]] = Decoder.partial[Map[Symbol, A]] {
    case PMap(m) => m
      .map { case (k, v) => decoderA(v).map(vv => k -> vv) }
      .toPResult
      .map(_.toMap)
  }

  implicit def mapKeyStringDecoder[A: TypeTag](implicit decoderA: Decoder[A]): Decoder[Map[String, A]] = Decoder.partial[Map[String, A]] {
    case PMap(m) => m
      .map { case (k, v) => decoderA(v).map(vv => k.name -> vv) }
      .toPResult
      .map(_.toMap)
  }

  implicit val bytesDecoder: Decoder[Array[Byte]] = Decoder.partial[Array[Byte]] {
    case PBytes(v) => valid(v)
  }

  implicit val pmapDecoder: Decoder[PMap] = Decoder.partial[PMap] {
    case pm: PMap => valid(pm)
  }

  implicit def optDecoder[A: Decoder]: Decoder[Option[A]] = new Decoder[Option[A]] {
    override def apply(v: PValue): PResult[Option[A]] = v match {
      case Null => valid(None)
      case _ => implicitly[Decoder[A]].apply(v).map(Some.apply)
    }
  }

  implicit def traversableDecoder[A: Decoder, T[_] <: Traversable[_]](implicit cbf: CanBuildFrom[Nothing, A, T[A]]): Decoder[T[A]] = new Decoder[T[A]] {
    override def apply(v: PValue): PResult[T[A]] = v match {
      case PArray(arr) =>
        val decoder = implicitly[Decoder[A]]
        catchNonFatal(arr.map(decoder.unsafe)).map(vector => {
          val b = cbf()
          vector.foreach(v => b += v)
          b.result()
        })
      case other => invalid(new IllegalStateException(s"Can not traverse over ${other.getClass}. Expected PArray."))
    }
  }
}

object DecoderInstances extends DecoderInstances
