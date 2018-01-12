package io.parsek.algebra

/**
  * @author Andrei Tupitcyn
  */
trait Semigroup[@specialized(Int, Long, Float, Double) A] extends Any with Serializable  {
  /**
    * Associative operation which combines two values.
    */
  def combine(x: A, y: A): A
}

object Semigroup {
  @inline def pure[A](f: (A, A) => A): Semigroup[A] = new Semigroup[A] {
    override def combine(x: A, y: A): A = f(x, y)
  }
}
