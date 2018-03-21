package io.parsek

import io.parsek.algebra.Empty
import org.scalatest.{FlatSpec, Inside, Matchers}

/**
  * @author Andrei Tupitcyn
  */
class PResultSpec extends FlatSpec with Matchers with Inside {
  "PResult" should "extract result" in {
    inside(PResult.valid(5)) {
      case PSuccessA(res) => res shouldBe 5
    }

    val ex = new RuntimeException
    inside(PResult.valid(5).withWarning(ex)) {
      case PSuccess(res, warns) =>
        res shouldBe 5
        warns shouldBe ex :: Nil
    }
  }

  it should "support monadic operations" in {
    val res1 = for {
      a <- PResult.valid(10)
      if a > 1
    } yield a * a

    res1 shouldBe PResult.valid(100)

    val res2 = for {
      a <- PResult.valid(10)
      if a > 10
    } yield a * a

    res2 shouldBe PResult.empty
  }
}
