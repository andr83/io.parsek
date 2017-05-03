package io.parsek

import java.time.Instant

/**
  * @author andr83
  */
sealed trait PValue extends Product with Serializable

object PValue {
  type FieldType = (String, PValue)
  private[parsek] final case class PBoolean(value: Boolean) extends PValue
  private[parsek] final case class PInt(value: Int) extends PValue
  private[parsek] final case class PLong(value: Long) extends PValue
  private[parsek] final case class PDouble(value: Double) extends PValue
  private[parsek] final case class PString(value: String) extends PValue
  private[parsek] final case class PTime(value: Instant) extends PValue
  private[parsek] final case class PBytes(value: Array[Byte]) extends PValue
  private[parsek] final case class PArray(value: Vector[PValue]) extends PValue
  private[parsek] final case class PMap(value: Map[String, PValue]) extends PValue
  private[parsek] final case object PNull extends PValue

  final val Null: PValue = PNull
  final val True: PValue = PBoolean(true)
  final val False: PValue = PBoolean(false)

  final def arr(values: PValue*): PValue = PArray(values.toVector)
  final def pmap(fields: FieldType*): PValue = PMap(fields.toMap)

  final def fromValues(values: Iterable[PValue]): PValue = PArray(values.toVector)
  final def fromFields(fields: Iterable[FieldType]): PValue = PMap(fields.toMap)

  final def fromBoolean(v: Boolean): PValue = if (v) True else False
  final def fromInt(v: Int): PValue = PInt(v)
  final def fromLong(v: Long): PValue = PLong(v)
  final def fromDouble(v: Double): PValue = PDouble(v)
  final def fromString(v: String): PValue = PString(v)
  final def fromInstant(v: Instant): PValue = PTime(v)
}
