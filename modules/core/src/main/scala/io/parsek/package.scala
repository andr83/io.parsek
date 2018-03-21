package io

import java.time.Instant

import io.parsek.PValue._
import io.parsek.implicits._
import io.parsek.optics.LensPath
import io.parsek.types._

/**
  * @author Andrei Tupitcyn
  */
package object parsek {
  type ThrowableNel = NonEmptyList[Throwable]

  @inline val root: LensPath = LensPath.root

  @inline def arr(values: PValue*): PValue = PValue.arr(values: _*)

  @inline def pmap(fields: PValue.FieldType*): PMap = PValue.pmap(fields: _*)

  def validate(root: PMap, schema: PStructType): PResult[PValue] = {
    val m = root.value
    schema.fields.map { case PStructField(name, dataType, nullable, required) =>
      m.get(name) match {
        case None | Some(PNull) =>
          if (nullable) {
            PResult.valid(Map(name -> PNull)).withWarning(NullField(name, s"Field ${name.name} is empty"))
          } else PResult.invalid(NullField(name, s"Field ${name.name} is empty in $m"))
        case Some(v) =>
          val validateResult = if (!required && nullable) {
            validateType(v, dataType).fold(nel => PResult.valid(PValue.Null).withWarnings(nel.toList), PResult.valid, PResult.empty)
          } else validateType(v, dataType)
          validateResult.map(vv => Map(name -> vv))
      }
    }.reduce(_.combine(_)).map(PValue.fromMap)
  }

  def validateType(value: PValue, dataType: PType): PResult[PValue] = dataType match {
    case PBooleanType => value match {
      case v: PBoolean => PResult.valid(v)
      case x => implicitly[Decoder[Boolean]].apply(x).map(PValue.fromBoolean)
    }
    case PIntType => value match {
      case v: PInt => PResult.valid(v)
      case x => implicitly[Decoder[Int]].apply(x).map(PValue.fromInt)
    }
    case PLongType => value match {
      case v: PLong => PResult.valid(v)
      case x => implicitly[Decoder[Long]].apply(x).map(PValue.fromLong)
    }
    case PDoubleType => value match {
      case v: PDouble => PResult.valid(v)
      case x => implicitly[Decoder[Double]].apply(x).map(PValue.fromDouble)
    }
    case PStringType => value match {
      case v: PString => PResult.valid(v)
      case x => implicitly[Decoder[String]].apply(x).map(PValue.fromString)
    }
    case PInstantType => value match {
      case v: PInstant => PResult.valid(v)
      case x => implicitly[Decoder[Instant]].apply(x).map(PValue.fromInstant)
    }
    case PDateType => value match {
      case v: PInstant => PResult.valid(v)
      case x => implicitly[Decoder[Instant]].apply(x).map(PValue.fromInstant)
    }
    case PBinaryType => value match {
      case v: PBytes => PResult.valid(v)
      case x => PResult.invalid(TypeCastFailure(s"Can not cast value $x to PBinaryType"))
    }
    case PArrayType(innerType) => value match {
      case v: PArray => PResult.valid(v) //ToDo: Add inner validation
      case x => PResult.invalid(TypeCastFailure(s"Can not cast value $x to PArrayType"))
    }
    case PMapType => value match {
      case v: PMap => PResult.valid(v)
      case x => PResult.invalid(TypeCastFailure(s"Can not cast value $x to PMapType"))
    }
    case scheme: PStructType => value match {
      case v: PMap => validate(v, scheme)
      case x => PResult.invalid(TypeCastFailure(s"Can not cast value $x to PMapType"))
    }
  }
}
