package io.parsek.jdbc

import java.sql.{Date, PreparedStatement, Timestamp}
import java.time.{Instant, LocalDateTime, ZoneId}

import io.parsek.instances.DecoderInstances._
import io.parsek.{Decoder, NullValue}

/**
  * @author Andrei Tupitcyn
  */

object PValueBinder {
  val byteBinder: PValueBinder = pure[Int]((stmt, index, x) => stmt.setByte(index, x.toByte), java.sql.Types.SMALLINT)
  val shortBinder: PValueBinder = pure[Int]((stmt, index, x) => stmt.setShort(index, x.toByte), java.sql.Types.SMALLINT)
  val intBinder: PValueBinder = pure[Int]((stmt, index, x) => stmt.setInt(index, x), java.sql.Types.INTEGER)
  val longBinder: PValueBinder = pure[Long]((stmt, index, x) => stmt.setLong(index, x), java.sql.Types.BIGINT)
  val floatBinder: PValueBinder = pure[Double]((stmt, index, x) => stmt.setFloat(index, x.toFloat), java.sql.Types.FLOAT)
  val doubleBinder: PValueBinder = pure[Double]((stmt, index, x) => stmt.setDouble(index, x), java.sql.Types.DOUBLE)
  val booleanBinder: PValueBinder = pure[Boolean]((stmt, index, x) => stmt.setBoolean(index, x), java.sql.Types.BOOLEAN)
  val instantBinder: PValueBinder = pure[Instant]((stmt, index, x) => stmt.setTimestamp(index, Timestamp.from(x)), java.sql.Types.TIMESTAMP)
  val dateBinder: PValueBinder = pure[Instant]((stmt, index, x) => stmt.setDate(index,
    Date.valueOf(LocalDateTime.ofInstant(x, ZoneId.systemDefault()).toLocalDate)), java.sql.Types.DATE)
  val blobBinder: PValueBinder = pure[Array[Byte]]((stmt, index, x) => {
    val blob = new javax.sql.rowset.serial.SerialBlob(x)
    stmt.setBlob(index, blob)
  }, java.sql.Types.BLOB)
  val stringBinder: PValueBinder = pure[String]((stmt, index, x) => stmt.setString(index, x), java.sql.Types.VARCHAR)

  def pure[A: Decoder](f: (PreparedStatement, Int, A) => Unit, sqlType: Int): PValueBinder = x => (stmt, index) =>
    implicitly[Decoder[A]].apply(x) match {
      case Right(v) => f(stmt, index, v)
      case Left(NullValue(_)) => stmt.setNull(index, sqlType)
      case Left(error) => throw error
    }

  def apply(sqlType: Int): PValueBinder = sqlType match {
    case java.sql.Types.BIT | java.sql.Types.SMALLINT | java.sql.Types.TINYINT | java.sql.Types.INTEGER => intBinder
    case java.sql.Types.BIGINT => longBinder
    case java.sql.Types.FLOAT | java.sql.Types.DOUBLE | java.sql.Types.REAL | java.sql.Types.DECIMAL => floatBinder
    case java.sql.Types.BOOLEAN => booleanBinder
    case java.sql.Types.TIMESTAMP | java.sql.Types.TIMESTAMP_WITH_TIMEZONE => instantBinder
    case java.sql.Types.DATE => dateBinder
    case java.sql.Types.BLOB => blobBinder
    case _ => stringBinder
  }
}
