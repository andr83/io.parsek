package io.parsek.optics

import io.parsek.implicits._
import io.parsek.{PResult, PValue, Transformation}

import scala.language.implicitConversions

/**
  * @author Andrei Tupitcyn
  */
case class Projection(transformations: Iterable[(Symbol, Transformation[PValue, PValue])])
  extends Transformation[PValue, PValue] {

  override def transform(source: PValue): PResult[PValue] = {
    transformations
      .map {
        case (toKey, t) => t.transform(source).map((pv: PValue) => toKey -> pv)
      }
      .toPResult
      .map(PValue.fromFields)
  }
}

object Projection {
  def apply(transformer: (Symbol, Transformation[PValue, PValue])*): Projection = Projection(transformer)
}
