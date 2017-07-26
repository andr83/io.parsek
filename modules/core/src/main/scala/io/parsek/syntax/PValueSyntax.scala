package io.parsek.syntax


import java.time.Instant

import io.parsek.PValue.PMap
import io.parsek.implicits._
import io.parsek.{Decoder, PValue, TraverseFailure}

import scala.language.implicitConversions


trait PValueSyntax {
  implicit final def pvalueSyntaxOps(v: PValue): PValueOps = new PValueOps(v)
}

/**
  * @author Andrei Tupitcyn
  */
final class PValueOps(val v: PValue) extends AnyVal {
  def as[A: Decoder](key: Symbol): Either[Throwable, A] = v match {
    case PMap(map) => implicitly[Decoder[A]].apply(map.getOrElse(key, PValue.Null))
    case other => throw TraverseFailure(s"Can not traverse to field ${key.name} in $other")
  }

  def opt: Option[PValue] = v match {
    case PValue.Null => None
    case other => Some(other)
  }

  def int: Int = as[Int].fold(e => throw e, identity)

  def long: Long = as[Long].fold(e => throw e, identity)

  def float: Float = as[Float].fold(e => throw e, identity)

  def double: Double = as[Double].fold(e => throw e, identity)

  def boolean: Boolean = as[Boolean].fold(e => throw e, identity)

  def string: String = as[String].fold(e => throw e, identity)

  def instant: Instant = as[Instant].fold(e => throw e, identity)

  def bytes: Array[Byte] = as[Array[Byte]].fold(e => throw e, identity)

  def arr[A: Decoder]: Vector[A] = {
    val d = implicitly[Decoder[A]]
    parr.map(r => d(r).fold(e => throw e, identity))
  }

  def parr: Vector[PValue] = as[Vector[PValue]].fold(e => throw e, identity)

  def as[A: Decoder]: Either[Throwable, A] = implicitly[Decoder[A]].apply(v)
}