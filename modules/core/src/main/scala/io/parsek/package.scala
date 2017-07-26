package io

import java.time.Instant

import cats.data.Validated.{Invalid, Valid, invalidNel, valid}
import cats.data.ValidatedNel
import cats.syntax.either._
import io.parsek.PValue._
import io.parsek.implicits._
import io.parsek.optics.PPath
import io.parsek.types._

import scala.collection.mutable

/**
  * @author Andrei Tupitcyn
  */
package object parsek {
  @inline val root: PPath = PPath.root

  @inline def arr(values: PValue*): PValue = PValue.arr(values: _*)

  @inline def pmap(fields: PValue.FieldType*): PMap = PValue.pmap(fields: _*)

  def validate(root: PMap, schema: PStructType, warnings: mutable.ArrayBuffer[Throwable] = mutable.ArrayBuffer.empty): ValidatedNel[Throwable, PValue] = {
    val m = root.value
    schema.fields.map { case PStructField(name, dataType, nullable) =>
      m.get(name) match {
        case None | Some(PNull) =>
          if (nullable) {
            warnings += NullField(name, s"Field ${name.name} is empty")
            valid(Map(name -> PNull))
          } else invalidNel(NullField(name, s"Field ${name.name} is empty in $m"))
        case Some(v) =>
          validateType(v, dataType, warnings) match {
            case Valid(vv) =>
              valid(Map(name -> vv))
            case inv@Invalid(nel) =>
              if (nullable) {
                warnings ++= nel.toList
                valid(Map(name -> PNull))
              } else inv
          }
      }
    }.reduce(_.combine(_)).map(PValue.fromMap)
  }

  def validateType(value: PValue, dataType: PType, warnings: mutable.ArrayBuffer[Throwable]): ValidatedNel[Throwable, PValue] = dataType match {
    case PBooleanType => value match {
      case v: PBoolean => valid(v)
      case x => implicitly[Decoder[Boolean]].apply(x).map(PValue.fromBoolean).toValidatedNel
    }
    case PIntType => value match {
      case v: PInt => valid(v)
      case x => implicitly[Decoder[Int]].apply(x).map(PValue.fromInt).toValidatedNel
    }
    case PLongType => value match {
      case v: PLong => valid(v)
      case x => implicitly[Decoder[Long]].apply(x).map(PValue.fromLong).toValidatedNel
    }
    case PDoubleType => value match {
      case v: PDouble => valid(v)
      case x => implicitly[Decoder[Double]].apply(x).map(PValue.fromDouble).toValidatedNel
    }
    case PStringType => value match {
      case v: PString => valid(v)
      case x => implicitly[Decoder[String]].apply(x).map(PValue.fromString).toValidatedNel
    }
    case PInstantType => value match {
      case v: PTime => valid(v)
      case x => implicitly[Decoder[Instant]].apply(x).map(PValue.fromInstant).toValidatedNel
    }
    case PBinaryType => value match {
      case v: PBytes => valid(v)
      case x => invalidNel(TypeCastFailure(s"Can not cast value $x to PBinaryType"))
    }
    case PArrayType => value match {
      case v: PArray => valid(v)
      case x => invalidNel(TypeCastFailure(s"Can not cast value $x to PArrayType"))
    }
    case PMapType => value match {
      case v: PMap => valid(v)
      case x => invalidNel(TypeCastFailure(s"Can not cast value $x to PMapType"))
    }
    case scheme: PStructType => value match {
      case v: PMap => validate(v, scheme, warnings)
      case x => invalidNel(TypeCastFailure(s"Can not cast value $x to PMapType"))
    }
  }
}
