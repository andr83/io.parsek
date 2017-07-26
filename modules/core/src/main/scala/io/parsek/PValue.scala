package io.parsek

import java.time.Instant

import scala.language.dynamics

/**
  * @author andr83
  */
sealed trait PValue extends Product with Serializable with Dynamic {

  import io.parsek.PValue.PMap

  def selectDynamic(field: String): PValue = at(Symbol(field))

  def at(key: Symbol): PValue = this match {
    case PMap(map) => map.getOrElse(key, PValue.Null)
    case other => throw TraverseFailure(s"Can not traverse to field ${key.name} in $other")
  }
}

object PValue {
  type FieldType = (Symbol, PValue)
  final val Null: PValue = PNull
  final val True: PValue = PBoolean(true)
  final val False: PValue = PBoolean(false)

  final def arr(values: PValue*): PValue = PArray(values.toVector)

  final def pmap(fields: FieldType*): PMap = PMap(fields.toMap)

  final def fromValues(values: Traversable[PValue]): PValue = PArray(values.toVector)

  final def fromFields(fields: Traversable[FieldType]): PValue = PMap(fields.toMap)

  final def fromMap(map: Map[Symbol, PValue]): PValue = PMap(map)

  final def fromBoolean(v: Boolean): PValue = if (v) True else False

  final def fromInt(v: Int): PValue = PInt(v)

  final def fromLong(v: Long): PValue = PLong(v)

  final def fromDouble(v: Double): PValue = PDouble(v)

  final def fromString(v: String): PValue = PString(v)

  final def fromInstant(v: Instant): PValue = PTime(v)

  final def fromBytes(v: Array[Byte]): PValue = PBytes(v)

  def apply[A : Encoder](a: A): PValue = implicitly[Encoder[A]].apply(a)

  final case class PBoolean(value: Boolean) extends PValue

  final case class PInt(value: Int) extends PValue

  final case class PLong(value: Long) extends PValue

  final case class PDouble(value: Double) extends PValue

  final case class PString(value: String) extends PValue

  final case class PTime(value: Instant) extends PValue

  final case class PBytes(value: Array[Byte]) extends PValue

  final case class PArray(value: Vector[PValue]) extends PValue

  final case class PMap(value: Map[Symbol, PValue]) extends PValue {
    def update(k: Symbol, v: PValue): PMap = PMap(value.updated(k, v))
  }

  private[parsek] final case object PNull extends PValue
}
