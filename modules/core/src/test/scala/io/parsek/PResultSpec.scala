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
}
