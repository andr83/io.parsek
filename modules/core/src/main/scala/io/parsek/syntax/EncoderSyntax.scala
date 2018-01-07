package io.parsek.syntax

import io.parsek.{Encoder, PValue}

import scala.language.implicitConversions

/**
  * @author Andrei Tupitcyn
  *         created on 06.10.17
  */
trait EncoderSyntax {
  implicit def encoderSyntaxOps[A: Encoder](a: A): EncoderOps[A] = new EncoderOps(a)
}

object EncoderSyntax extends EncoderSyntax

class EncoderOps[A](val a: A) extends AnyVal {
  def toPValue(implicit encoder: Encoder[A]): PValue = encoder(a)
}
