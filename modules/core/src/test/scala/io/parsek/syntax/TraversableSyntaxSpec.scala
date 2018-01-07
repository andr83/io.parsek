package io.parsek.syntax

import cats.data.NonEmptyList
import io.parsek.syntax.TraversableSyntax._
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Andrei Tupitcyn
  */
class TraversableSyntaxSpec extends FlatSpec with Matchers {
  "Traversable of either " must " separate on Left and Right parts" in {
    val coll: Seq[Either[Int, Int]] = Seq(Left(1), Right(2), Left(3), Right(4))
    val (left, right) = coll.separate
    left shouldBe Seq(1, 3)
    right shouldBe Seq(2, 4)
  }

  "Traversable of either " must " converted to Left[C[_]] " in {
    val coll: Seq[Either[Int, Int]] = Seq(Left(1), Right(2), Left(3), Right(4))
    val res = coll.toEither
    res shouldBe Left(Seq(1, 3))
  }

  "Traversable of either NEL " must " transform to Left[NonEmptyList] " in {
    val coll: Seq[Either[NonEmptyList[Int], Int]] = Seq(Left(NonEmptyList.of(1)), Right(2), Left(NonEmptyList.of(3)), Right(4))
    val res = coll.toEitherNel
    res shouldBe Left(NonEmptyList.of(1, 3))
  }

  it must "transform to Right" in {
    val coll: Seq[Either[NonEmptyList[Int], Int]] = Seq(Right(1), Right(2), Right(3), Right(4))
    val res = coll.toEitherNel
    res shouldBe Right(Seq(1, 2, 3, 4))
  }
}
