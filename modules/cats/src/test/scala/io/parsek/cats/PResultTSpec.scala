package io.parsek.cats

import cats.instances.future.catsStdInstancesForFuture
import io.parsek.PResult
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
  * @author Andrei Tupitcyn
  */
class PResultTSpec extends FlatSpec with Matchers {
  "PResultT transformer" should "work with Future" in {
    val fp: Future[PResult[String]] = Future.successful(PResult.valid("Hello"))
    val f: Future[String] = Future.successful("World")
    val p: PResult[String] = PResult.valid("!")

    val resT: PResultT[Future, String] = for {
      s1 <- PResultT(fp)
      s2 <- PResultT.liftF(f)
      s3 <- PResultT.fromPResult(p)
    } yield s"$s1 $s2 $s3"

    val res = Await.result(resT.value, Duration.Inf)
    res shouldBe PResult.valid("Hello World !")
  }
}
