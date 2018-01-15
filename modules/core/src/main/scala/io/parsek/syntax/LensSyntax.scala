package io.parsek.syntax

import io.parsek.optics.Getter
import io.parsek.{Encoder, PResult, PValue}
import scala.language.implicitConversions

/**
  * @author Andrei Tupitcyn
  */
trait LensSyntax {
  implicit def getterAtoGetterPValue[A: Encoder](getterA: Getter[PValue, A]): Getter[PValue, PValue] = new Getter[PValue, PValue] {
    override def get(s: PValue): PResult[PValue] = getterA.get(s).map(implicitly[Encoder[A]].apply)
  }
}
