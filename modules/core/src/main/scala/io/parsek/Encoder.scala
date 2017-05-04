package io.parsek

/**
  * @author Andrei Tupitcyn
  */
trait Encoder[A] extends Serializable {
  def apply(a: A): PValue
}

object Encoder {
  def pure[A](f: A => PValue): Encoder[A] = new Encoder[A] {
    def apply(a: A): PValue = f(a)
  }
}
