package io.parsek

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
    dataType: PType
  ) extends PType

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

}
