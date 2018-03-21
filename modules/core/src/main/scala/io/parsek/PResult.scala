package io.parsek

import java.util.NoSuchElementException

import io.parsek.algebra.{Empty, EmptyInstances1, Semigroup}

import scala.util.{Failure, Success, Try}

/**
  *
  * @author Andrei Tupitcyn
  */
sealed abstract class PResult[+A] {
  self =>

  val isError: Boolean

  @inline def unsafe[AA >: A](implicit empty: Empty[AA]): AA =
    fold(nel => throw nel.head, identity, empty.empty.getOrElse(throw PResult.noSuchElementException))

  @inline def bimap[B](fe: ThrowableNel => ThrowableNel, fa: A => B): PResult[B] =
    this match {
      case PSuccess(a, warnings) => PSuccess(fa(a), warnings)
      case PError(errors)        => PError(fe(errors))
      case e: PEmpty             => e
    }

  @inline def map[B](fa: A => B): PResult[B] =
    this match {
      case PSuccess(a, warnings) => PSuccess(fa(a), warnings)
      case e: PError             => e
      case e: PEmpty             => e
    }

  @inline def mapIn[B](fa: PartialFunction[A, B]): PResult[B] =
    this match {
      case PSuccess(a, warnings) if fa.isDefinedAt(a) =>
        PSuccess(fa(a), warnings)
      case PSuccessA(a) =>
        PError(TypeCastFailure(s"Can not apply partial map function to $a"))
      case e: PError => e
      case e: PEmpty => e
    }

  @inline def errorMap(fe: ThrowableNel => ThrowableNel): PResult[A] =
    this match {
      case PError(errors) => PError(fe(errors))
      case ok             => ok
    }

  @inline def filter(p: A => Boolean): PResult[A] =
    this match {
      case s @ PSuccess(a, warnings) => if (p(a)) s else PEmpty(warnings)
      case e: PError                 => e
      case e: PEmpty                 => e
    }

  @inline def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): PResult[B] = self filter p map f
    def flatMap[B](f: A => PResult[B]): PResult[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter =
      new WithFilter(x => p(x) && q(x))
  }

  @inline def foreach[U](f: A => U): Unit =
    this match {
      case PSuccessA(a) => f(a)
      case _            => ()
    }

  @inline def flatMap[B](fa: A => PResult[B]): PResult[B] =
    this match {
      case PSuccess(a, warnings) if warnings.nonEmpty =>
        fa(a).withWarnings(warnings)
      case PSuccessA(a) => fa(a)
      case e: PError    => e
      case e: PEmpty    => e
    }

  @inline def flatMapIn[B](fa: PartialFunction[A, PResult[B]]): PResult[B] =
    this match {
      case PSuccess(a, warnings) if fa.isDefinedAt(a) =>
        fa(a).withWarnings(warnings)
      case PSuccessA(a) =>
        PError(TypeCastFailure(s"Can not apply partial flatMap function to $a"))
      case e: PError => e
      case e: PEmpty => e
    }

  def withWarning(error: Throwable): PResult[A] =
    this match {
      case PSuccess(a, warnings) => PSuccess(a, warnings :+ error)
      case PEmpty(warnings)      => PEmpty(warnings :+ error)
      case e: PError             => e
    }

  def withWarnings(errors: Seq[Throwable]): PResult[A] =
    this match {
      case PSuccess(a, warnings) => PSuccess(a, warnings ++ errors)
      case e: PError             => e
      case PEmpty(warnings)      => PEmpty(warnings ++ errors)
    }

  def withWarnings(errors: NonEmptyList[Throwable]): PResult[A] =
    this match {
      case PSuccess(a, warnings) => PSuccess(a, warnings ++ errors.toList)
      case e: PError             => e
      case PEmpty(warnings)      => PEmpty(warnings ++ errors.toList)
    }

  @inline def combine[AA >: A](that: PResult[AA])(implicit AA: Semigroup[AA]): PResult[AA] =
    (this, that) match {
      case (PSuccess(a, w1), PSuccess(b, w2)) =>
        PSuccess(AA.combine(a, b), w1 ++ w2)
      case (PError(e1), PError(e2))        => PError(e1 ++ e2.toList)
      case (s: PSuccess[A], PResult.empty) => s
      case (PResult.empty, s: PResult[AA]) => s
      case (PSuccess(a, w1), PEmpty(w2))   => PSuccess(a, w1 ++ w2)
      case (PEmpty(w1), PSuccess(b, w2))   => PSuccess(b, w1 ++ w2)
      case (PError(_), _)                  => this
      case (_, PError(_))                  => that
      case (PEmpty(w1), PEmpty(w2))        => PEmpty(w1 ++ w2)
    }

  @inline def getOrElse[B >: A](default: => B): B =
    fold(_ => default, identity, default)

  @inline def fold[B](fe: ThrowableNel => B, fa: A => B, fn: => B): B =
    this match {
      case PSuccess(a, _) => fa(a)
      case PError(errors) => fe(errors)
      case _: PEmpty      => fn
    }

  @inline def orElse[B >: A](alternative: => PResult[B]): PResult[B] =
    this match {
      case x: PSuccess[A] => x
      case e: PError      => alternative
      case e: PEmpty      => alternative
    }

  @inline def toOption: Option[A] = fold(_ => None, Option.apply, None)

  @inline def toEither[AA >: A](implicit empty: Empty[AA]): Either[Throwable, AA] =
    fold(
      e => Left(e.head),
      Right.apply,
      empty.empty.fold[Either[Throwable, AA]](Left(PResult.noSuchElementException))(Right.apply)
    )

  @inline def toEitherNel[AA >: A](implicit empty: Empty[AA]): Either[NonEmptyList[Throwable], AA] =
    fold(
      Left.apply,
      Right.apply,
      empty.empty
        .fold[Either[NonEmptyList[Throwable], AA]](Left(NonEmptyList.one(PResult.noSuchElementException)))(Right.apply)
    )
}

object PResult extends EmptyInstances1 {
  val noSuchElementException = new NoSuchElementException

  val empty: PEmpty = PEmpty()

  def invalid(errors: ThrowableNel): PResult[Nothing] = PError(errors)

  def catchNonFatal[A : Empty](f: => A): PResult[A] =
    try {
      valid(f)
    } catch {
      case scala.util.control.NonFatal(t) => invalid(t)
    }

  def valid[A](a: A)(implicit empty: Empty[A]): PResult[A] = if (empty.isEmpty(a)) PResult.empty else PSuccess(a)

  def invalid(head: Throwable, tail: Throwable*): PResult[Nothing] = PResult.invalid(NonEmptyList.of(head, tail: _*))

  def fromTry[A](t: Try[A]): PResult[A] =
    t match {
      case Failure(e) => invalid(e)
      case Success(v) => valid(v)
    }
}

case class PSuccess[A](private val value: A, warnings: Seq[Throwable] = Nil) extends PResult[A] {
  val isError: Boolean = false
  def get: A = value
}

object PSuccessA {
  def unapply[A](res: PSuccess[A]): Option[A] = res.toOption
}

case class PEmpty(warnings: Seq[Throwable] = Nil) extends PResult[Nothing] {
  override val isError: Boolean = false
}

case class PError(errors: NonEmptyList[Throwable]) extends PResult[Nothing] {
  val isError: Boolean = true
  def error: Throwable = errors.head
}

object PError {
  def apply(error: Throwable*): PError =
    PError(NonEmptyList.of(error.head, error.tail: _*))
}
