package io.parsek

import io.parsek.algebra.Semigroup

import scala.util.{Failure, Success, Try}

/**
  *
  * @author Andrei Tupitcyn
  */
sealed abstract class PResult[+A] {
  val isError: Boolean

  def withWarning(error: Throwable): PResult[A] =
    this match {
      case PSuccess(a, warnings) => PSuccess(a, warnings :+ error)
      case e: PError => e
    }

  @inline def unsafe: A = fold(nel=> throw nel.head, identity)

  @inline def bimap[B](fe: ThrowableNel => ThrowableNel, fa: A => B): PResult[B] =
    this match {
      case PSuccess(a, warnings) => PSuccess(fa(a), warnings)
      case PError(errors) => PError(fe(errors))
    }

  @inline def map[B](fa: A => B): PResult[B] =
    this match {
      case PSuccess(a, warnings) => PSuccess(fa(a), warnings)
      case e: PError => e
    }

  @inline def errorMap(fe: ThrowableNel => ThrowableNel): PResult[A] =
    this match {
      case PError(errors) => PError(fe(errors))
      case ok => ok
    }

  @inline def foreach(f: A => Unit): Unit =
    this match {
      case PSuccess(a, _) => f(a)
      case _ => ()
    }

  @inline def flatMap[B](fa: A => PResult[B]): PResult[B] =
    this match {
      case PSuccess(a, warnings) if warnings.nonEmpty => fa(a).withWarnings(warnings)
      case PSuccess(a, _) => fa(a)
      case e: PError => e
    }

  def withWarnings(errors: Seq[Throwable]): PResult[A] =
    this match {
      case PSuccess(a, warnings) => PSuccess(a, warnings ++ errors)
      case e: PError => e
    }

  @inline def combine[AA >: A](that: PResult[AA])(implicit AA: Semigroup[AA]): PResult[AA] =
    (this, that) match {
      case (PSuccess(a, w1), PSuccess(b, w2)) => PSuccess(AA.combine(a, b), w1 ++ w2)
      case (PError(e1), PError(e2)) => PError(e1 ++ e2.toList)
      case (PError(_), _) => this
      case (_, PError(_)) => that
    }

  @inline def getOrElse[B >: A](default: => B): B = fold(_=> default, identity)

  @inline def fold[B](fe: ThrowableNel => B, fa: A => B): B =
    this match {
      case PSuccess(a, _) => fa(a)
      case PError(errors) => fe(errors)
    }

  @inline def orElse[B >: A](alternative: => PResult[B]): PResult[B] =
    this match {
      case x: PSuccess[A] => x
      case e: PError => alternative
    }

  @inline def toOption: Option[A] = fold(_ => None, Option.apply)

  @inline def toEither: Either[Throwable, A] = fold(e => Left(e.head), Right.apply)

  @inline def toEitherNel: Either[NonEmptyList[Throwable], A] = fold(Left.apply, Right.apply)
}

object PResult {
  def invalid(errors: ThrowableNel): PResult[Nothing] = PError(errors)

  def catchNonFatal[A](f: => A): PResult[A] =
    try {
      valid(f)
    } catch {
      case scala.util.control.NonFatal(t) => invalid(t)
    }

  def valid[A](a: A): PResult[A] = PSuccess(a)

  def invalid(head: Throwable, tail: Throwable*): PResult[Nothing] = PError(NonEmptyList.of(head, tail: _*))

  def fromTry[A](t: Try[A]): PResult[A] =
    t match {
      case Failure(e) => invalid(e)
      case Success(v) => valid(v)
    }
}

case class PSuccess[A](private val value: A, warnings: Seq[Throwable] = Seq.empty) extends PResult[A] {
  val isError: Boolean = false
  def get: A = value
}

case class PError(errors: NonEmptyList[Throwable]) extends PResult[Nothing] {
  val isError: Boolean = true
  def error: Throwable = errors.head
}

object PError {
  def apply(error: Throwable*): PError = PError(NonEmptyList.of(error.head, error.tail: _*))
}