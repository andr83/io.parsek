package io.parsek.optics

import io.parsek.PResult

/**
  * @author Andrei Tupitcyn
  */
abstract class PTraversal[S, T, A, B] {
  def modify(f: A => B): S => PResult[T]

  def getAll(s: S): Traversable[A]

  @inline final def set(b: B)(s: S): PResult[T] = modify(_ => b)(s)
}

object Traversal {
  def apply[S, A](_getAll: S => Traversable[A])(_modify: (A => A) => (S => PResult[S])): Traversal[S, A] =
    new PTraversal[S, S, A, A] {
      override def getAll(s: S): Traversable[A] = _getAll(s)

      override def modify(f: A => A): S => PResult[S] = _modify(f)
    }
}
