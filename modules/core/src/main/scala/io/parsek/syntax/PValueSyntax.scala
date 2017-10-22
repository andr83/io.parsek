package io.parsek.syntax


import io.parsek.PValue._
import io.parsek._
import io.parsek.implicits._
import io.parsek.types.{PValueTyped, _}

import scala.language.implicitConversions


trait PValueSyntax {
  implicit final def pvalueSyntaxOps(v: PValue): PValueOps = new PValueOps(v)
}

/**
  * @author Andrei Tupitcyn
  */
final class PValueOps(val value: PValue) extends AnyVal {
  self =>
  def as[A: Decoder](key: Symbol): PResult[A] = value match {
    case PMap(map) => implicitly[Decoder[A]].apply(map.getOrElse(key, PValue.Null))
    case other => throw TraverseFailure(s"Can not traverse to field ${key.name} in $other")
  }

  def asUnsafe[A: Decoder](key: Symbol): A = implicitly[Decoder[A]].unsafe(value)

  def opt: Option[PValue] = value match {
    case PValue.Null => None
    case other => Some(other)
  }

  def to[A: Decoder]: A = asUnsafe[A]

  def asUnsafe[A: Decoder]: A = as[A].fold[A](nel => throw nel.head, identity)

  def as[A: Decoder]: PResult[A] = implicitly[Decoder[A]].apply(value)

  def map[A: Decoder, B: Encoder](f: A => B): PResult[PValue] = {
    val d = implicitly[Decoder[A]]
    val e = implicitly[Encoder[B]]

    value match {
      case PNull => PResult.valid(PValue.Null)
      case other => d(other).map(f.andThen(e.apply))
    }
  }

  def mapValues[A: Decoder, B: Encoder](f: A => B): PResult[PValue] = {
    val d = implicitly[Decoder[A]]
    val e = implicitly[Encoder[B]]

    value match {
      case PArray(arr) =>
        arr
          .map(r => d(r).map(f.andThen(e.apply)))
          .toPResult
          .map(PValue.fromValues)
      case PMap(map) =>
        map
          .map { case (k, v) => d(v).map(f.andThen(r => k -> e(r))) }
          .toPResult
          .map(PValue.fromFields)
      case PNull => PResult.valid(PValue.Null)
      case other => d(other).map(f.andThen(e.apply))
    }
  }

  def mapWithKey[A: Decoder, B: Encoder](f: (Symbol, A) => (Symbol, B)): PResult[PValue] = {
    val d = implicitly[Decoder[A]]
    val e = implicitly[Encoder[B]]

    value match {
      case PMap(map) =>
        val res = map
          .map(kv => {
            d(kv._2).fold(_ => kv, v => {
              val (k2, v2) = f(kv._1, v)
              k2 -> e(v2)
            })
          })
        PResult.valid(PValue.fromMap(res))
      case other => PResult.valid(other)
    }
  }

  def transform[A: Decoder, B: Encoder](f: A => PResult[B]): PResult[PValue] = {
    val d = implicitly[Decoder[A]]
    val e = implicitly[Encoder[B]]

    value match {
      case PArray(arr) =>
        arr
          .map(r => d(r).flatMap(v => f(v).map(e.apply)))
          .toPResult
          .map(PValue.fromValues)
      case PMap(map) =>
        map
          .map { case (k, pv) => d(pv).flatMap(v => f(v).map(r => k -> e(r))) }
          .toPResult
          .map(PValue.fromFields)
      case PNull => PResult.valid(PValue.Null)
      case other => d(other).flatMap(v => f(v).map(e.apply))
    }
  }

  def findAndMap[A: Decoder, B: Encoder](p: (Symbol, PValue) => Boolean, f: (Symbol, A) => (Symbol, B)): PValue = value match {
    case pm: PMap => PValueOps.traverseAndMap(p, f)(pm)
    case other => other
  }

  def typed: PValueTyped = PValueTyped(value, PType(value))

  def withType(t: PType): PValueTyped = PValueTyped(value, t)
}

object PValueOps {
  private def traverseAndMap[A: Decoder, B: Encoder](p: (Symbol, PValue) => Boolean, f: (Symbol, A) => (Symbol, B))(pm: PMap): PMap = {
    PMap(pm.value.map { case (k, v) =>
      if (p(k, v)) {
        val d = implicitly[Decoder[A]]
        val e = implicitly[Encoder[B]]
        d(v).fold(_ => k -> v, a => {
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