package io.parsek

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

  it should "zipWith" in {
    val pRes1 = PResult.valid(10)
    val pRes2 = PResult.valid("coins")

    pRes1.zipWith(pRes2)((p1, p2) => p1.toString + p2).unsafe shouldEqual "10coins"
  }

  it should "sequence" in {
    PResult.sequence(Seq(PResult.valid(10), PResult.valid(11))) shouldEqual PResult.valid(Seq(10, 11))
  }
}
