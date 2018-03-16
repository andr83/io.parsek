package io.parsek.cats

import cats.{Applicative, Functor, Monad}
import io.parsek.{PError, PResult, PSuccess}

/**
  * Monad transformer for PResult
  * @author Andrei Tupitcyn
  */
final case class PResultT[F[_], A](value: F[PResult[A]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): PResultT[F, B] =
    PResultT(F.map(value)(_.map(f)))

  def flatMap[B](f: A => PResultT[F, B])(implicit M: Monad[F]): PResultT[F, B] =
    flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[PResult[B]])(implicit F: Monad[F]): PResultT[F, B] =
    PResultT(F.flatMap(value) {
      case PSuccess(a, Nil)   => f(a)
      case PSuccess(a, warns) => F.map(f(a))(_.withWarnings(warns))
      case PError(errors)     => F.pure[PResult[B]](PError(errors))
    })
}

object PResultT {
  def liftF[F[_], A](fa: F[A])(implicit F: Functor[F]): PResultT[F, A] = PResultT(F.map(fa)(PResult.valid))
  def fromPResult[F[_], A](result: PResult[A])(implicit F: Applicative[F]): PResultT[F, A] = PResultT(F.pure(result))
}
