package io.parsek.jdbc.generic.instances

import java.sql.{Blob, Date, Timestamp, Types}
import java.time.{Instant, LocalDate, LocalDateTime}
import io.parsek.jdbc.ParameterTypeMeta
import io.parsek.implicits._

/**
  * @author andr83
  */
trait ParameterTypeMetaInstances {
  implicit val booleanTypeMeta: ParameterTypeMeta[Boolean] = ParameterTypeMeta[Boolean](Types.BOOLEAN)

  implicit val byteTypeMeta: ParameterTypeMeta[Byte] = ParameterTypeMeta[Byte](Types.TINYINT)

  implicit val shortTypeMeta: ParameterTypeMeta[Short] = ParameterTypeMeta[Short](Types.SMALLINT)

  implicit val intTypeMeta: ParameterTypeMeta[Int] = ParameterTypeMeta[Int](Types.INTEGER)

  implicit val longTypeMeta: ParameterTypeMeta[Long] = ParameterTypeMeta[Long](Types.BIGINT)

  implicit val floatTypeMeta: ParameterTypeMeta[Float] = ParameterTypeMeta[Float](Types.FLOAT)

  implicit val doubleTypeMeta: ParameterTypeMeta[Double] = ParameterTypeMeta[Double](Types.DOUBLE)

  implicit val charTypeMeta: ParameterTypeMeta[Char] = ParameterTypeMeta[Char](Types.CHAR)

  implicit val stringTypeMeta: ParameterTypeMeta[String] = ParameterTypeMeta[String](Types.VARCHAR)

  implicit val sqlTimestampTypeMeta: ParameterTypeMeta[Timestamp] = ParameterTypeMeta[java.sql.Timestamp](Types.TIMESTAMP)

  implicit val sqlDateTypeMeta: ParameterTypeMeta[Date] = ParameterTypeMeta[java.sql.Date](Types.DATE)

  implicit val localDateTypeMeta: ParameterTypeMeta[LocalDate] = ParameterTypeMeta[LocalDate](Types.DATE)

  implicit val localDateTimeTypeMeta: ParameterTypeMeta[LocalDateTime] = ParameterTypeMeta[LocalDateTime](Types.TIMESTAMP)

  implicit val instantTypeMeta: ParameterTypeMeta[Instant] = ParameterTypeMeta[Instant](Types.TIMESTAMP)

  implicit val bytesTypeMeta: ParameterTypeMeta[Array[Byte]] = ParameterTypeMeta[Array[Byte]](Types.LONGVARBINARY)

  implicit val blobTypeMeta: ParameterTypeMeta[Blob] = ParameterTypeMeta[java.sql.Blob](Types.BLOB)
}
