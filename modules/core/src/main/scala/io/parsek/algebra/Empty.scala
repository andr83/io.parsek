package io.parsek.algebra

import scala.collection.generic.CanBuildFrom

/**
  * @author Andrei Tupitcyn
  */
trait Empty[A] {
  def empty: Option[A]
  def isEmpty(a: A): Boolean
}

trait EmptyInstances0 {
  implicit def defaultEmpty[A]: Empty[A] = new Empty[A] {
    override def empty: Option[A] = None

    override def isEmpty(a: A): Boolean = a == null
  }
}

trait EmptyInstances1 extends EmptyInstances0 {
  implicit def optionEmpty[A]: Empty[Option[A]] = Empty(None)
  implicit def mapEmpty[K, V]: Empty[Map[K, V]] = Empty(Map.empty)
  implicit def canBuildFromEmpty[C[_], A](implicit cbf: CanBuildFrom[C[A], A, C[A]]): Empty[C[A]] = Empty(cbf().result())
}

object Empty extends EmptyInstances1 {

  def apply[A](emptyA: A): Empty[A] = new Empty[A]() {
    override def empty: Option[A] = Option(emptyA)

    override def isEmpty(a: A): Boolean = a == emptyA
  }
}