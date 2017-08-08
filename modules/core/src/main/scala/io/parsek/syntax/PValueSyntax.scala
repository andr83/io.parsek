package io.parsek.syntax


import java.time.Instant
import cats.syntax.either._

import io.parsek.PValue.{PMap, PArray}
import io.parsek.PValue.{PArray, PMap, PNull}
import io.parsek.implicits._
import io.parsek.{Decoder, Encoder, PValue, TraverseFailure}

import scala.language.implicitConversions


trait PValueSyntax {
  implicit final def pvalueSyntaxOps(v: PValue): PValueOps = new PValueOps(v)
}

/**
  * @author Andrei Tupitcyn
  */
final class PValueOps(val value: PValue) extends AnyVal {
  def as[A: Decoder](key: Symbol): Either[Throwable, A] = value match {
    case PMap(map) => implicitly[Decoder[A]].apply(map.getOrElse(key, PValue.Null))
    case other => throw TraverseFailure(s"Can not traverse to field ${key.name} in $other")
  }

  def as[A: Decoder]: Either[Throwable, A] = implicitly[Decoder[A]].apply(value)

  def asUnsafe[A: Decoder](key: Symbol): A = implicitly[Decoder[A]].unsafe(value)

  def asUnsafe[A: Decoder]: A = as[A].fold(e => throw e, identity)

  def opt: Option[PValue] = value match {
    case PValue.Null => None
    case other => Some(other)
  }

  def int: Int = asUnsafe[Int]

  def long: Long = asUnsafe[Long]

  def float: Float = asUnsafe[Float]

  def double: Double = asUnsafe[Double]

  def boolean: Boolean = asUnsafe[Boolean]

  def string: String = asUnsafe[String]

  def instant: Instant = asUnsafe[Instant]

  def bytes: Array[Byte] = asUnsafe[Array[Byte]]

  def arr[A: Decoder]: Vector[A] = {
    val d = implicitly[Decoder[A]]
    parr.map(r => d(r).fold(e => throw e, identity))
  }

  def parr: Vector[PValue] = asUnsafe[Vector[PValue]]

  def mapUnsafe[A: Decoder, B: Encoder](f: A => B): PValue = {
    val d = implicitly[Decoder[A]]
    val e = implicitly[Encoder[B]]

    value match {
      case PArray(arr) => PArray(arr.map(r => e(f(d.unsafe(r)))))
      case PMap(map) => PMap(map.mapValues(r => e(f(d.unsafe(r)))))
      case PNull => PValue.Null
      case other => e(f(d.unsafe(other)))
    }
  }

  def mapK[A: Decoder, B: Encoder](f: (Symbol, A) => (Symbol, B)): PValue = {
    val d = implicitly[Decoder[A]]
    val e = implicitly[Encoder[B]]

    value match {
      case PMap(map) => PMap(map.map(kv => {
        d(kv._2).fold(_ => kv._1 -> kv._2, v => {
          val (k2, v2) = f(kv._1, v)
          k2 -> e(v2)
        })
      }))
      case other => other
    }
  }

  def findAndMap[A : Decoder, B: Encoder](p: (Symbol,PValue) => Boolean, f: (Symbol,A) => (Symbol,B)): PValue = value match {
    case pm: PMap => PValueOps.traverseAndMap(p, f)(pm)
    case other => other
  }
}

object PValueOps {
  private def traverseAndMap[A : Decoder, B: Encoder](p: (Symbol,PValue) => Boolean, f: (Symbol,A) => (Symbol,B))(pm: PMap): PMap = {
    PMap(pm.value.map{ case (k, v) =>
      if(p(k, v)) {
        val d = implicitly[Decoder[A]]
        val e = implicitly[Encoder[B]]
        d(v).fold(_=> k -> v, a=> {
          val (kb, vb) = f(k, a)
          kb -> e(vb)
        })
      } else v match {
        case m: PMap => k -> traverseAndMap[A, B](p, f)(m)
        case PArray(arr) =>
          k -> PArray(arr.map {
            case m: PMap => traverseAndMap[A, B](p, f)(m)
            case other => other
          })
        case other => k -> other
      }
    })
  }
}