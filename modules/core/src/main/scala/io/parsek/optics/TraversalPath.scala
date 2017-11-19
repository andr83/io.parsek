package io.parsek.optics

import io.parsek.{PResult, PValue}

/**
  * @author Andrei Tupitcyn
  */
case class TraversalPath[A](private val traversal: Traversal[PValue, A]) {
  def getAll(s: PValue): Traversable[A] = traversal.getAll(s)

  def modify(f: A => A): PValue => PResult[PValue] = traversal.modify(f)

  def set(b: A): PValue => PResult[PValue] = traversal.set(b)

  def map(f: A => A): LensPath = LensPath(Lens[PValue, PValue](traversal.modify(f))(a => s => traversal.modify(f)(s)))
}
