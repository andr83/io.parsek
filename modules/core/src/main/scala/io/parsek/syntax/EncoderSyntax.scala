package io.parsek.syntax

import io.parsek.{Encoder, PValue}

import scala.language.implicitConversions

/**
  * @author andr83 
  *         created on 06.10.17
  */
trait EncoderSyntax {
  implicit def encoderSyntaxOps[T : Encoder](t: T): EncoderOps[T] = new EncoderOps(t)
}

object EncoderSyntax extends EncoderSyntax

class EncoderOps[T](val obj: T) extends AnyVal {
  def toPValue(implicit encoder: Encoder[T]): PValue = encoder(obj)
}
