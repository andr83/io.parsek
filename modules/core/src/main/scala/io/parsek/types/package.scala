package io.parsek

import io.parsek.PValue._

/**
  * @author Andrei Tupitcyn
  */
package object types {

  abstract sealed class PType

  case object PBooleanType extends PType

  case object PIntType extends PType

  case object PLongType extends PType

  case object PDoubleType extends PType

  case object PStringType extends PType

  case object PInstantType extends PType

  case object PDateType extends PType

  case object PBinaryType extends PType

  case class PArrayType(
    dataType: Option[PType] = None
  ) extends PType

  object PArrayType {
    def apply(dataType: PType): PArrayType = PArrayType(Some(dataType))
  }

  case object PMapType extends PType

  case class PStructField(
    name: Symbol,
    dataType: PType,
    nullable: Boolean = true
  )

  case class PStructType(fields: Array[PStructField]) extends PType {
    def add(field: PStructField): PStructType = PStructType(fields.filter(_.name != field.name) :+ field)
    def add(name: Symbol, dataType: PType, nullable: Boolean = true): PStructType = add(PStructField(name, dataType, nullable))
  }

  object PStructType {
    def apply(fields: Seq[PStructField]): PStructType = PStructType(fields.toArray)
  }

  object PType {
    def apply(value: PValue): PType = value match {
      case _: PBoolean => PBooleanType
      case _: PInt => PIntType
      case _: PLong => PLongType
      case _: PString => PStringType
      case _: PDouble => PDoubleType
      case _: PInstant => PInstantType
      case _: PBytes => PBinaryType
      case PArray(arr) => PArrayType(arr.foldLeft(arr.headOption.map(PType.apply).map(Some.apply).getOrElse(None)) {
        case (None, _) => None
        case (o @ Some(t), v) =>
          if (PType(v) != t) None else o
        })
      case PMap(m) => PStructType(m map {
        case  (k, v) => PStructField(k, PType(v))
      } toSeq)
    }
  }

  trait PValueTyped {
    val valueType: PType
    val value: PValue
  }

  object PValueTyped {
    def apply(v: PValue, vType: PType): PValueTyped = new PValueTyped {
      override val valueType = vType
      override val value = v
    }
  }
}
