package io.parsek.jdbc.instances

import java.time.{Instant, LocalDate, LocalDateTime}

import io.parsek.jdbc.ColumnReader

/**
  * @author Andrei Tupitcyn
  */
trait ColumnReaderInstances {
  implicit val shortColumnReader: ColumnReader[Short] = ColumnReader.pure(_.getShort(_), _.getShort(_))
  implicit val intColumnReader: ColumnReader[Int] = ColumnReader.pure(_.getInt(_), _.getInt(_))
  implicit val longColumnReader: ColumnReader[Long] = ColumnReader.pure(_.getLong(_), _.getLong(_))
  implicit val floatColumnReader: ColumnReader[Float] = ColumnReader.pure(_.getFloat(_), _.getFloat(_))
  implicit val doubleColumnReader: ColumnReader[Double] = ColumnReader.pure(_.getDouble(_), _.getDouble(_))
  implicit val booleanColumnReader: ColumnReader[Boolean] = ColumnReader.pure(_.getBoolean(_), _.getBoolean(_))
  implicit val stringColumnReader: ColumnReader[String] = ColumnReader.pure(_.getString(_), _.getString(_))
  implicit val instantColumnReader: ColumnReader[Instant] = ColumnReader.pure(_.getTimestamp(_).toInstant, _.getTimestamp(_).toInstant)
  implicit val sqlDateColumnReader: ColumnReader[java.sql.Date] = ColumnReader.pure(_.getDate(_), _.getDate(_))
  implicit val localDateColumnReader: ColumnReader[LocalDate] = ColumnReader.pure(_.getDate(_).toLocalDate, _.getDate(_).toLocalDate)
  implicit val localDateTimeColumnReader: ColumnReader[LocalDateTime] = ColumnReader.pure(_.getTimestamp(_).toLocalDateTime, _.getTimestamp(_).toLocalDateTime)
  implicit val byteArrayColumnReader: ColumnReader[Array[Byte]] = ColumnReader.pure(_.getBytes(_), _.getBytes(_))
}
