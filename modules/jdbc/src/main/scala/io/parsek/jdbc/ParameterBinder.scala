package io.parsek.jdbc

import java.sql.{Blob, Date, PreparedStatement, Timestamp}
import java.time.Instant

/**
  * @author Andrei Tupitcyn
  */
object ParameterBinder {
  val byteBinder: ValueBinder[Byte] = pure[Byte]((stmt, index, x) => stmt.setByte(index, x), java.sql.Types.SMALLINT)
  val shortBinder: ValueBinder[Short] = pure[Short]((stmt, index, x) => stmt.setShort(index, x), java.sql.Types.SMALLINT)
  val intBinder: ValueBinder[Int] = pure[Int]((stmt, index, x) => stmt.setInt(index, x), java.sql.Types.INTEGER)
  val longBinder: ValueBinder[Long] = pure[Long]((stmt, index, x) => stmt.setLong(index, x), java.sql.Types.BIGINT)
  val floatBinder: ValueBinder[Float] = pure[Float]((stmt, index, x) => stmt.setFloat(index, x), java.sql.Types.FLOAT)
  val doubleBinder: ValueBinder[Double] = pure[Double]((stmt, index, x) => stmt.setDouble(index, x), java.sql.Types.DOUBLE)
  val booleanBinder: ValueBinder[Boolean] = pure[Boolean]((stmt, index, x) => stmt.setBoolean(index, x), java.sql.Types.BOOLEAN)
  val instantBinder: ValueBinder[Instant] = pure[Instant]((stmt, index, x) => stmt.setTimestamp(index, Timestamp.from(x)), java.sql.Types.TIMESTAMP)
  val dateBinder: ValueBinder[Date] = pure[Date]((stmt, index, x) => stmt.setDate(index, x), java.sql.Types.DATE)
  val blobBinder: ValueBinder[Blob] = pure[Blob]((stmt, index, x) => stmt.setBlob(index, x), java.sql.Types.BLOB)
  val stringBinder: ValueBinder[String] = pure[String]((stmt, index, x) => stmt.setString(index, x), java.sql.Types.VARCHAR)

  def pure[A](f: (PreparedStatement, Int, A) => Unit, sqlType: Int): ValueBinder[A] = x => (stmt, index) =>
    if (x == null) {
      stmt.setNull(index, sqlType)
    } else f(stmt, index, x)

  def apply(param: Any): ParameterBinder = param match {
    case b: ParameterBinder => b
    case v: Long => longBinder(v)
    case v: Int => intBinder(v)
    case v: Short => shortBinder(v)
    case v: Byte => byteBinder(v)
    case v: Float => floatBinder(v)
    case v: Double => doubleBinder(v)
    case v: Boolean => booleanBinder(v)
    case v: Instant => instantBinder(v)
    case v: String => stringBinder(v)
    case other =>
      throw new IllegalArgumentException(s"Can not create ParameterBinder for $other")
  }
}
