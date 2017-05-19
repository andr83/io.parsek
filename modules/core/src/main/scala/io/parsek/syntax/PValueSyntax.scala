package io.parsek.syntax


import io.parsek.{Decoder, PValue}

import scala.language.implicitConversions


trait PValueSyntax {
  implicit final def pvalueSyntaxOps(v: PValue): PValueOps = new PValueOps(v)
}

/**
  * @author Andrei Tupitcyn
  */
final class PValueOps(val v: PValue) extends AnyVal {
  def as[A: Decoder]: Either[Throwable, A] = implicitly[Decoder[A]].apply(v)
  def opt: Option[PValue] = v match {
    case PValue.Null => None
    case other => Some(other)
  }
}