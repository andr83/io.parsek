package io.parsek.jdbc.instances

import java.sql
import java.sql.{PreparedStatement, Types}
import java.time.{Instant, LocalDate, LocalDateTime}

import io.parsek.PValue
import io.parsek.PValue._
import io.parsek.implicits._
import io.parsek.jdbc.ValueBinder.pure
import io.parsek.jdbc.{ParameterBinder, ParameterTypeMeta, ValueBinder}
import io.parsek.jdbc.instances.parameterBinder.nullParameterBinder
import io.parsek.types._

/**
  * @author Andrei Tupitcyn
  */

trait ValueBinderInstances extends ParameterTypeMetaInstances {
  implicit val byteValueBinder: ValueBinder[Byte] = pure[Byte]((stmt, index, x) => {stmt.setByte(index, x); index + 1})
  implicit val shortValueBinder: ValueBinder[Short] = pure[Short]((stmt, index, x) => {stmt.setShort(index, x); index + 1})
  implicit val intValueBinder: ValueBinder[Int] = pure[Int]((stmt, index, x) => {stmt.setInt(index, x); index + 1})
  implicit val longValueBinder: ValueBinder[Long] = pure[Long]((stmt, index, x) => {stmt.setLong(index, x); index + 1})
  implicit val floatValueBinder: ValueBinder[Float] = pure[Float]((stmt, index, x) => {stmt.setFloat(index, x); index + 1})
  implicit val doubleValueBinder: ValueBinder[Double] = pure[Double]((stmt, index, x) => {stmt.setDouble(index, x); index + 1})
  implicit val booleanValueBinder: ValueBinder[Boolean] = pure[Boolean]((stmt, index, x) => {stmt.setBoolean(index, x); index + 1})
  implicit val instantValueBinder: ValueBinder[Instant] = pure[Instant]((stmt, index, x) => {stmt.setTimestamp(index, java.sql.Timestamp.from(x)); index + 1})
  implicit val sqlDateValueBinder: ValueBinder[java.sql.Date] = pure[java.sql.Date]((stmt, index, x) => {stmt.setDate(index, x); index + 1})
  implicit val localDateValueBinder: ValueBinder[LocalDate] = pure[LocalDate]((stmt, index, x) => {stmt.setDate(index, java.sql.Date.valueOf(x)); index + 1})
  implicit val localDateTimeValueBinder: ValueBinder[LocalDateTime] = pure[LocalDateTime]((stmt, index, x) => {stmt.setTimestamp(index, java.sql.Timestamp.valueOf(x)); index + 1})
  implicit val blobValueBinder: ValueBinder[java.sql.Blob] = pure[java.sql.Blob]((stmt, index, x) => {stmt.setBlob(index, x); index + 1})
  implicit val charValueBinder: ValueBinder[Char] = pure[Char]((stmt, index, x) => {stmt.setString(index, x.toString); index + 1})
  implicit val stringValueBinder: ValueBinder[String] = pure[String]((stmt, index, x) => {stmt.setString(index, x); index + 1})
  implicit val byteArrayValueBinder: ValueBinder[Array[Byte]] = pure[Array[Byte]]((stmt, index, x) => {stmt.setBytes(index, x); index + 1})

  implicit def sqlArrayFromTraversableValueBinder[A, T <: Traversable[A]](implicit meta: ParameterTypeMeta[A]): ValueBinder[T] = {
    new ValueBinder[T] {
      override def apply(t: T): ParameterBinder =
        new ParameterBinder {
          def bind(stmt: PreparedStatement, index: Int): Int = if (t == null) {
            stmt.setNull(index, Types.ARRAY)
            index + 1
          } else {
            val arr = stmt.getConnection.createArrayOf(meta.sqlType, t.map {
              case v: PValue =>
                meta.decoder.unsafe(v).asInstanceOf[AnyRef]
              case other: AnyRef => other
            }.toArray)
            stmt.setArray(index, arr)
            index + 1
          }
        }
    }
  }

  implicit val sqlArrayValueValueBinder: ValueBinder[java.sql.Array]  = new ValueBinder[java.sql.Array] {
    override def apply(arr: sql.Array): ParameterBinder = new ParameterBinder {
      override def bind(stmt: PreparedStatement, index: Int): Int =
        if (arr == null) {
          stmt.setNull(index, Types.ARRAY)
          index + 1
        } else {
          stmt.setArray(index, arr)
          index + 1
        }
    }
  }

  implicit def arrayValueValueBinder[A](implicit meta: ParameterTypeMeta[A]): ValueBinder[Array[A]] = {
    new ValueBinder[Array[A]] {
      override def apply(t: Array[A]): ParameterBinder =
        new ParameterBinder {
          def bind(stmt: PreparedStatement, index: Int): Int = if (t == null) {
            stmt.setNull(index, Types.ARRAY)
            index + 1
          } else {
            val arr = stmt.getConnection.createArrayOf(meta.sqlType, t.asInstanceOf[Array[AnyRef]])
            stmt.setArray(index, arr)
            index + 1
          }
        }
    }
  }

  implicit def optionalValueBinder[A](implicit binder: ValueBinder[A], meta: ParameterTypeMeta[A]): ValueBinder[Option[A]] = {
    new ValueBinder[Option[A]] {
      override def apply(o: Option[A]): ParameterBinder = o match {
        case Some(x) => binder(x)
        case _ => nullParameterBinder[A](meta)
      }
    }
  }

  def traversableValueBinder[A, T <: Traversable[A]](implicit binder: ValueBinder[A]): ValueBinder[T] = {
    new ValueBinder[T] {
      override def apply(t: T): ParameterBinder =
        new ParameterBinder {
          override def bind(stmt: PreparedStatement, index: Int): Int = {
            t.map(binder.apply).foldLeft(index) {
              case (nextIndex, b) => b.bind(stmt, nextIndex)
            }
          }
        }
    }
  }

  implicit def seqValueBinder[A](implicit binder: ValueBinder[A]): ValueBinder[Seq[A]] = traversableValueBinder[A, Seq[A]](binder)
  implicit def listValueBinder[A](implicit binder: ValueBinder[A]): ValueBinder[List[A]] = traversableValueBinder[A, List[A]](binder)
  implicit def vectorValueBinder[A](implicit binder: ValueBinder[A]): ValueBinder[Vector[A]] = traversableValueBinder[A, Vector[A]](binder)

  implicit val pvalueValueBinder: ValueBinder[PValue] = new ValueBinder[PValue] {
    override def apply(x: PValue): ParameterBinder = pvalueTypedValueBinder.apply(x.typed)
  }

  implicit val pvalueTypedValueBinder: ValueBinder[PValueTyped] = new ValueBinder[PValueTyped] {
    override def apply(x: PValueTyped): ParameterBinder = valueBinder(x.valueType).apply(x.value)
  }

  def valueBinder(valueType: PType): ValueBinder[PValue] = valueType match {
    case PBooleanType => ValueBinder.wrap(booleanValueBinder)
    case PIntType => ValueBinder.wrap(intValueBinder)
    case PLongType => ValueBinder.wrap(longValueBinder)
    case PDoubleType => ValueBinder.wrap(doubleValueBinder)
    case PStringType => ValueBinder.wrap(stringValueBinder)
    case PInstantType => ValueBinder.wrap(instantValueBinder)
    case PDateType => ValueBinder.wrap(localDateValueBinder)
    case PBinaryType => ValueBinder.wrap(byteArrayValueBinder)
    case PArrayType(innerType) => innerType match {
      case None => new ValueBinder[PValue] {
        override def apply(pv: PValue): ParameterBinder = pv match {
          case v: PArray => new ParameterBinder {
            override def bind(stmt: PreparedStatement, index: Int): Int = {
              stmt.setArray(index, stmt.getConnection.createArrayOf("NULL", v.value.map(pvalue2AnyRef).toArray))
              index + 1
            }
          }
          case other => throw new IllegalArgumentException(s"Expected PArray value to bind to PreparedStatement but got $other")
        }
      }
      case Some(PBooleanType) => ValueBinder.wrap(sqlArrayFromTraversableValueBinder[Boolean, Vector[Boolean]](booleanTypeMeta))
      case Some(PIntType) => ValueBinder.wrap(sqlArrayFromTraversableValueBinder[Int, Vector[Int]](intTypeMeta))
      case Some(PLongType) => ValueBinder.wrap(sqlArrayFromTraversableValueBinder[Long, Vector[Long]](longTypeMeta))
      case Some(PDoubleType) => ValueBinder.wrap(sqlArrayFromTraversableValueBinder[Double, Vector[Double]](doubleTypeMeta))
      case Some(PInstantType) => ValueBinder.wrap(sqlArrayFromTraversableValueBinder[Instant, Vector[Instant]](instantTypeMeta))
      case Some(PStringType) => ValueBinder.wrap(sqlArrayFromTraversableValueBinder[String, Vector[String]](stringTypeMeta))
      case Some(PDateType) => ValueBinder.wrap(sqlArrayFromTraversableValueBinder[java.sql.Date, Vector[java.sql.Date]](sqlDateTypeMeta))
      case Some(PBinaryType) => ValueBinder.wrap(sqlArrayFromTraversableValueBinder[Array[Byte], Vector[Array[Byte]]](bytesTypeMeta))
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
    case PDate(v) => java.sql.Date.valueOf(v)
    case PBytes(v) => v
    case PArray(v) => v.map(pvalue2AnyRef)
    case PMap(m) => m.map { case (k, v) => k.name -> pvalue2AnyRef(v) }
  }
}
