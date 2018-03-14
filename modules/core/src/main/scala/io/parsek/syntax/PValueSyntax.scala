package io.parsek.syntax


import io.parsek.PValue._
import io.parsek._
import io.parsek.implicits._
import io.parsek.types.{PValueTyped, _}

import scala.language.implicitConversions

/** Extending PValue with additional ops */
trait PValueSyntax {
  implicit final def pvalueSyntaxOps(v: PValue): PValueOps = new PValueOps(v)
  implicit final def a2PValue[A : Encoder](a: A): PValue = implicitly[Encoder[A]].apply(a)
}

/**
  * @author Andrei Tupitcyn
  */
final class PValueOps(val value: PValue) extends AnyVal {
  self =>

  /** Get inner value by `key` and safe decode it to type `A` */
  def as[A: Decoder](key: Symbol): PResult[A] = value match {
    case PMap(map) => implicitly[Decoder[A]].apply(map.getOrElse(key, PValue.Null))
    case other => throw TraverseFailure(s"Can not traverse to field ${key.name} in $other")
  }

  /** Get inner value by `key` and unsafe decode it to type `A` */
  def asUnsafe[A: Decoder](key: Symbol): A = implicitly[Decoder[A]].unsafe(value)

  /** Convert `PValue` to `Option[PValue]`. `PValue.Null` will convert to `None` */
  def opt: Option[PValue] = value match {
    case PValue.Null => None
    case other => Some(other)
  }

  /** Alias to `unsafe` method */
  def to[A: Decoder]: A = asUnsafe[A]

  /** Unsafe convert value to type `A` */
  def asUnsafe[A: Decoder]: A = as[A].fold[A](nel => throw nel.head, identity)

  /** Safe convert value to type `A` */
  def as[A: Decoder]: PResult[A] = implicitly[Decoder[A]].apply(value)

  /** Map value with function `f` */
  def map[A: Decoder, B: Encoder](f: A => B): PResult[PValue] = {
    val d = implicitly[Decoder[A]]
    val e = implicitly[Encoder[B]]

    value match {
      case PNull => PResult.valid(PValue.Null)
      case other => d(other).map(f.andThen(e.apply))
    }
  }

  /** Map value with function `f`. If underlying type is PArray or PStruct function `f` will call recursively. */
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

  /** Map value with function `f` taken `(Symbol, A)` as arguments. Applying only to PMap values. */
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

  /** Map value with possible failed result. */
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

  def typed: PValueTyped = PValueTyped(value, PType(value))

  def withType(t: PType): PValueTyped = PValueTyped(value, t)
}