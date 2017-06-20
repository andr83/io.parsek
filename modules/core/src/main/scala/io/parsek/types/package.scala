package io.parsek

/**
  * @author Andrei Tupitcyn
  */
package object types {

  abstract sealed class PType

  case class PStructField(
    name: Symbol,
    dataType: PType,
    nullable: Boolean = true
  )

  case class PStructType(fields: Array[PStructField]) extends PType

  case object PBooleanType extends PType

  case object PIntType extends PType

  case object PLongType extends PType

  case object PDoubleType extends PType

  case object PStringType extends PType

  case object PInstantType extends PType

  case object PBinaryType extends PType

  case object PArrayType extends PType

  case object PMapType extends PType

  object PStructType {
    def apply(fields: Seq[PStructField]): PStructType = PStructType(fields.toArray)
  }

}
