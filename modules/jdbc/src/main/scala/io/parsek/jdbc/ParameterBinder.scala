package io.parsek.jdbc

import java.sql.{Blob, Date, PreparedStatement, Timestamp}
import java.time.Instant

import io.parsek.PValue
import io.parsek.PValue._

/**
  * @author Andrei Tupitcyn
  */

trait ParameterBinder extends ((PreparedStatement, Int) => Unit)

trait ValueBinder[A] extends (A => ParameterBinder)

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
  val vectorBinder: ValueBinder[Vector[PValue]] = pure[Vector[PValue]]((stmt, index, x) => {
    val arr = stmt.getConnection.createArrayOf("VARCHAR", x.map {
      case PString(v) => v.toString
      case PInt(v) => v.toString
      case PLong(v) => v.toString
      case PDouble(v) => v.toString
      case PBoolean(v) => v.toString
      case PInstant(v) => v.toString
      case _ => throw new IllegalArgumentException(s"JDBC array can contain only simple values")
    }.toArray)
    stmt.setArray(index, arr)
  }, java.sql.Types.ARRAY)

  def pure[A](f: (PreparedStatement, Int, A) => Unit, sqlType: Int): ValueBinder[A] = new ValueBinder[A] {
    override def apply(a: A): ParameterBinder = new ParameterBinder {
      override def apply(stmt: PreparedStatement, index: Int): Unit =
        if (a == null) {
          stmt.setNull(index, sqlType)
        } else f(stmt, index, a)
    }
  }

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
    case v: Vector[PValue @unchecked] => vectorBinder(v)
    case other =>
      throw new IllegalArgumentException(s"Can not create ParameterBinder for $other")
  }
}
