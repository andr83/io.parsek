package io.parsek.instances

import cats.{Applicative, Monad}
import io.parsek._

/**
  * @author andr83
  */
trait PResultInstances {

  implicit def presultApplicative = new Applicative[PResult] {
    override def pure[A](x: A): PResult[A] = PSuccess(x)

    override def ap[A, B](ff: PResult[A => B])(fa: PResult[A]): PResult[B] = ???
  }

  implicit def presultMonad = new Monad[PResult] {
    override def flatMap[A, B](fa: PResult[A])(f: (A) => PResult[B]): PResult[B] = ???

    override def tailRecM[A, B](a: A)(f: (A) => PResult[Either[A, B]]): PResult[B] = ???

    override def pure[A](x: A): PResult[A] = ???
  }
}
