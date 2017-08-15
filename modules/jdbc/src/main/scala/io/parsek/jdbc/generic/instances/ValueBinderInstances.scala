package io.parsek.jdbc.generic.instances

import java.sql.{PreparedStatement, Types}
import java.time.{Instant, LocalDate, LocalDateTime}

import io.parsek.PValue._
import io.parsek.implicits._
import io.parsek.jdbc.{ParameterBinder, ParameterTypeMeta, ValueBinder, _}
import ValueBinder.{nullParameterBinder, pure}
import io.parsek.PValue
import io.parsek.types._

/**
  * @author Andrei Tupitcyn
  */

trait ValueBinderInstances extends ParameterTypeMetaInstances {
  implicit val byteBinder: ValueBinder[Byte] = pure[Byte]((stmt, index, x) => stmt.setByte(index, x))
  implicit val shortBinder: ValueBinder[Short] = pure[Short]((stmt, index, x) => stmt.setShort(index, x))
  implicit val intBinder: ValueBinder[Int] = pure[Int]((stmt, index, x) => stmt.setInt(index, x))
  implicit val longBinder: ValueBinder[Long] = pure[Long]((stmt, index, x) => stmt.setLong(index, x))
  implicit val floatBinder: ValueBinder[Float] = pure[Float]((stmt, index, x) => stmt.setFloat(index, x))
  implicit val doubleBinder: ValueBinder[Double] = pure[Double]((stmt, index, x) => stmt.setDouble(index, x))
  implicit val booleanBinder: ValueBinder[Boolean] = pure[Boolean]((stmt, index, x) => stmt.setBoolean(index, x))
  implicit val instantBinder: ValueBinder[Instant] = pure[Instant]((stmt, index, x) => stmt.setTimestamp(index, java.sql.Timestamp.from(x)))
  implicit val sqlDateBinder: ValueBinder[java.sql.Date] = pure[java.sql.Date]((stmt, index, x) => stmt.setDate(index, x))
  implicit val localDateBinder: ValueBinder[LocalDate] = pure[LocalDate]((stmt, index, x) => stmt.setDate(index, java.sql.Date.valueOf(x)))
  implicit val localDateTimeBinder: ValueBinder[LocalDateTime] = pure[LocalDateTime]((stmt, index, x) => stmt.setTimestamp(index, java.sql.Timestamp.valueOf(x)))
  implicit val blobBinder: ValueBinder[java.sql.Blob] = pure[java.sql.Blob]((stmt, index, x) => stmt.setBlob(index, x))
  implicit val charBinder: ValueBinder[Char] = pure[Char]((stmt, index, x) => stmt.setString(index, x.toString))
  implicit val stringBinder: ValueBinder[String] = pure[String]((stmt, index, x) => stmt.setString(index, x))
  implicit val byteArrayBinder: ValueBinder[Array[Byte]] = pure[Array[Byte]]((stmt, index, x) => stmt.setBytes(index, x))

  implicit def optionalBinder[A : ValueBinder : ParameterTypeMeta]: ValueBinder[Option[A]] = {
    val binder = implicitly[ValueBinder[A]]
    new ValueBinder[Option[A]] {
      override def apply(o: Option[A]): ParameterBinder = o match {
        case None => nullParameterBinder[A]
        case Some(x) => binder(x)
      }
    }
  }

  implicit def traversableBinder[A, T <: Traversable[A]](implicit meta: ParameterTypeMeta[A]): ValueBinder[T] = {
    new ValueBinder[T] {
      override def apply(t: T): ParameterBinder =
        new ParameterBinder {
          def bind(stmt: PreparedStatement, index: Int): Unit = if (t == null) {
            stmt.setNull(index, Types.ARRAY)
          } else {
            val arr = stmt.getConnection.createArrayOf(meta.sqlType, t.map {
              case v: PValue =>
                meta.decoder.unsafe(v).asInstanceOf[AnyRef]
              case other: AnyRef => other
            }.toArray)
            stmt.setArray(index, arr)
          }
        }
    }
  }

  implicit val pvalueBinder: ValueBinder[PValue] = new ValueBinder[PValue] {
    override def apply(x: PValue): ParameterBinder = pvalueTypedBinder.apply(x.typed)
  }

  implicit val pvalueTypedBinder: ValueBinder[PValueTyped] = new ValueBinder[PValueTyped] {
    override def apply(x: PValueTyped): ParameterBinder = valueBinder(x.valueType)(x.value)
  }

  def valueBinder(valueType: PType): ValueBinder[PValue] = valueType match {
    case PBooleanType => ValueBinder.wrap(booleanBinder)
    case PIntType => ValueBinder.wrap(intBinder)
    case PLongType => ValueBinder.wrap(longBinder)
    case PDoubleType => ValueBinder.wrap(doubleBinder)
    case PStringType => ValueBinder.wrap(stringBinder)
    case PInstantType => ValueBinder.wrap(instantBinder)
    case PDateType => ValueBinder.wrap(instantBinder)
    case PBinaryType => ValueBinder.wrap(byteArrayBinder)
    case PArrayType(innerType) => innerType match {
        case None => new ValueBinder[PValue] {
          override def apply(pv: PValue): ParameterBinder = pv match {
            case v: PArray => new ParameterBinder {
              override def bind(stmt: PreparedStatement, index: Int): Unit =
                stmt.setArray(index, stmt.getConnection.createArrayOf("NULL", v.value.map(pvalue2AnyRef).toArray))
            }
          }
        }
        case Some(PBooleanType) => ValueBinder.wrap(traversableBinder[Boolean, Vector[Boolean]](booleanTypeMeta))
        case Some(PIntType) => ValueBinder.wrap(traversableBinder[Int, Vector[Int]](intTypeMeta))
        case Some(PLongType) => ValueBinder.wrap(traversableBinder[Long, Vector[Long]](longTypeMeta))
        case Some(PDoubleType) => ValueBinder.wrap(traversableBinder[Double, Vector[Double]](doubleTypeMeta))
        case Some(PInstantType) => ValueBinder.wrap(traversableBinder[Instant, Vector[Instant]](instantTypeMeta))
        case Some(PStringType) => ValueBinder.wrap(traversableBinder[String, Vector[String]](stringTypeMeta))
        case Some(PDateType) => ValueBinder.wrap(traversableBinder[java.sql.Date, Vector[java.sql.Date]](sqlDateTypeMeta))
        case Some(PBinaryType) => ValueBinder.wrap(traversableBinder[Array[Byte], Vector[Array[Byte]]](bytesTypeMeta))
        case Some(unsupportedType) => throw new IllegalStateException(s"Cannot bind PArrayType($unsupportedType) value to JDBC PreparedStatement")
      }
    case unsupportedType => throw new IllegalStateException(s"Cannot bind $unsupportedType value to JDBC PreparedStatement")
  }

  def pvalue2AnyRef(pv: PValue): AnyRef = pv match {
    case PNull => null
    case PBoolean(v) => new java.lang.Boolean(v)
    case PInt(v) => new java.lang.Integer(v)
    case PLong(v) => new java.lang.Long(v)
    case PString(v) => v
    case PDouble(v) => new java.lang.Double(v)
    case PInstant(v) => java.sql.Timestamp.from(v)
    case PBytes(v) => v
    case PArray(v) => v.map(pvalue2AnyRef)
    case PMap(m) => m.map { case (k, v) => k.name -> pvalue2AnyRef(v) }
  }
}
