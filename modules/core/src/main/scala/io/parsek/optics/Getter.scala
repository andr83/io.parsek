package io.parsek.optics

import io.parsek.PResult

/**
  * [[Getter]] is an extractor for type A from type S
  *
  * @author Andrei Tupitcyn
  */
trait Getter[S, A] {
  /** get the target of a [[Getter]] */
  def get(s: S): PResult[A]
}

object Getter {
  def apply[S, A](_get: S => PResult[A]): Getter[S, A] = new Getter[S, A] {
    override def get(s: S): PResult[A] = _get(s)
  }
}
