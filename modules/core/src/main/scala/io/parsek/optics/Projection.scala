package io.parsek.optics

import cats.data.NonEmptyList
import cats.implicits._
import io.parsek.PValue

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * @author Andrei Tupitcyn
  */
case class Projection(validators: Iterable[(Symbol, PValidationNel)]) {
  val validation: PValidationNel = {
    Validation[PValue, NonEmptyList[Throwable], PValue](rec => {
      lazy val failures = mutable.ArrayBuffer.empty[NonEmptyList[Throwable]]
      val resSuccess = mutable.Map.empty[Symbol, PValue]

      val it = validators.iterator
      while (it.hasNext) {
        val (toKey, validator) = it.next()
        validator.get(rec) match {
          case Right(v) =>
            resSuccess += toKey -> v
          case Left(errList) =>
            failures += errList
        }
      }

      if (failures.nonEmpty) {
        Left[NonEmptyList[Throwable], PValue](failures.reduce(_ |+| _))
      } else {
        Right[NonEmptyList[Throwable], PValue](PValue.fromMap(resSuccess.toMap))
      }
    })(s => o => o)
  }

  def apply(p: PValue): NonEmptyList[Throwable] Either PValue = validation.get(p)
}

object Projection {
  def apply(validator: (Symbol, PValidationNel)*): Projection = Projection(validator)

  implicit def projectionToValidation(p: Projection): PValidationNel = p.validation
}
