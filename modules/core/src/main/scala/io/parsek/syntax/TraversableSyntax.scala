package io.parsek.syntax

import io.parsek._

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * @author Andrei Tupitcyn
  *         created on 20.09.17
  */
trait TraversableSyntax0 {

  implicit class TraversableEitherOps[L, R, C[_] <: Traversable[_]](underlying: C[Either[L, R]])(
    implicit cbfL: CanBuildFrom[Nothing, L, C[L]],
    cbfR: CanBuildFrom[Nothing, R, C[R]]
  ) {
    def toEither: Either[C[L], C[R]] = {
      val (l, r) = underlying.separate
      if (l.nonEmpty) {
        Left(l)
      } else {
        Right[C[L], C[R]](r)
      }
    }

    def separate: (C[L], C[R]) = {
      val (lb, rb) = underlying.foldLeft((cbfL(), cbfR())) {
        case ((lBuilder, rBuilder), Right(r: R@unchecked)) => (lBuilder, rBuilder += r)
        case ((lBuilder, rBuilder), Left(l: L@unchecked)) => (lBuilder += l, rBuilder)
        case _ => throw new IllegalStateException
      }
      (lb.result(), rb.result())
    }

    def toEitherNel: Either[NonEmptyList[L], C[R]] = {
      val (l, r) = underlying.separate
      if (l.nonEmpty) {
        Left(NonEmptyList.fromListUnsafe(l.toList.asInstanceOf[List[L]]))
      } else {
        Right[NonEmptyList[L], C[R]](r)
      }
    }
  }

}

trait TraversableSyntax extends TraversableSyntax0 {

  implicit class TraversableNelEitherOps[L, R, C[_] <: Traversable[_]](underlying: C[Either[NonEmptyList[L], R]])
    (
      implicit cbfL: CanBuildFrom[Nothing, L, C[L]],
      cbfR: CanBuildFrom[Nothing, R, C[R]]
    ) {
    def toEitherNel: Either[NonEmptyList[L], C[R]] = {
      val (l, r) = underlying.separate
      if (l.nonEmpty) {
        Left(NonEmptyList.fromListUnsafe(l.toList.asInstanceOf[List[L]]))
      } else {
        Right[NonEmptyList[L], C[R]](r)
      }
    }

    def separate: (C[L], C[R]) = {
      val (lb, rb) = underlying.foldLeft((cbfL(), cbfR())) {
        case ((lBuilder, rBuilder), Right(r: R@unchecked)) => (lBuilder, rBuilder += r)
        case ((lBuilder, rBuilder), Left(nel: NonEmptyList[L])) =>
          nel.toList.foreach(l => lBuilder += l)
          (lBuilder, rBuilder)
        case _ => throw new IllegalStateException
      }
      (lb.result(), rb.result())
    }
  }

  implicit class TraversablePResultOps[R, C[_] <: Traversable[_]](underlying: C[PResult[R]])(
    implicit cbfR: CanBuildFrom[Nothing, R, C[R]],
    cbfE: CanBuildFrom[Nothing, Throwable, C[Throwable]]
  ) {
    def toPResult: PResult[C[R]] = {
      val (errors, values) = this.separate
      if (errors.nonEmpty) {
        PResult.invalid(NonEmptyList.fromListUnsafe(errors.toList.asInstanceOf[List[Throwable]]))
      } else {
        PResult.valid(values)
      }
    }

    def separate: (C[Throwable], C[R]) = {
      val (lb, rb) = underlying.foldLeft((cbfE(), cbfR())) {
        case ((lBuilder, rBuilder), PSuccess(r: R@unchecked, _)) => (lBuilder, rBuilder += r)
        case ((lBuilder, rBuilder), PError(nel)) =>
          nel.toList.foreach(lBuilder += _)
          (lBuilder, rBuilder)
        case ((lBuilder, rBuilder), e: PEmpty) => (lBuilder, rBuilder)
        case _ => throw new IllegalStateException
      }
      (lb.result(), rb.result())
    }
  }

}

object TraversableSyntax extends TraversableSyntax
