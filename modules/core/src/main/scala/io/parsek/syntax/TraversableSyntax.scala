package io.parsek.syntax

import cats.data.NonEmptyList

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * @author andr83 
  *         created on 20.09.17
  */
trait TraversableSyntax0 {

  implicit class TraversableEitherOps[L, R, C[_] <: Traversable[_]](underlying: C[Either[L, R]])(
    implicit cbfL: CanBuildFrom[Nothing, L, C[L]],
    cbfR: CanBuildFrom[Nothing, R, C[R]]
  ) {
    def separate: (C[L], C[R]) = {
      val (lb, rb) = underlying.foldLeft((cbfL(), cbfR())) {
        case ((lBuilder, rBuilder), Right(r: R@unchecked)) => (lBuilder, rBuilder += r)
        case ((lBuilder, rBuilder), Left(l: L@unchecked)) => (lBuilder += l, rBuilder)
      }
      (lb.result(), rb.result())
    }

    def toEither: Either[C[L], C[R]] = {
      val (l, r) = underlying.separate
      if (l.nonEmpty) {
        Left(l)
      } else {
        Right[C[L], C[R]](r)
      }
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
    def separate: (C[L], C[R]) = {
      val (lb, rb) = underlying.foldLeft((cbfL(), cbfR())) {
        case ((lBuilder, rBuilder), Right(r: R @unchecked)) => (lBuilder, rBuilder += r)
        case ((lBuilder, rBuilder), Left(nel: NonEmptyList[L])) =>
          nel.toList.foreach(l => lBuilder += l)
          (lBuilder, rBuilder)
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

object TraversableSyntax extends TraversableSyntax
