package io.parsek

/**
  * @author andr83
  */
trait Transformation[A, B] {
  def transform(a: A): PResult[B]
}

object Transformation {
  def apply[A, B](f: A => PResult[B]): Transformation[A, B] = new Transformation[A, B] {
    override def transform(a: A): PResult[B] = f(a)
  }
}