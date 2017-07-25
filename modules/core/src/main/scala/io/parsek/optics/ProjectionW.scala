package io.parsek.optics

import cats.data.NonEmptyList
import cats.implicits._
import io.parsek.PValue

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * @author Andrei Tupitcyn
  *         Projection with warning
  */
case class ProjectionW(validators: Iterable[(Symbol, PValidationNelW)]) {
  val validation: Validation[PValue, NonEmptyList[Throwable], (Seq[Throwable], PValue)] = {
    Validation[PValue, NonEmptyList[Throwable], (Seq[Throwable], PValue)](rec => {
      val failures = mutable.ArrayBuffer.empty[NonEmptyList[Throwable]]
      val warnings = mutable.ArrayBuffer.empty[Throwable]
      val resSuccess = mutable.Map.empty[Symbol, PValue]

      val it = validators.iterator
      while (it.hasNext) {
        val (toKey, validator) = it.next()
        validator.get(rec) match {
          case Right((errors, v)) =>
            warnings ++= errors
            resSuccess += toKey -> v
          case Left(errList) =>
            failures += errList
        }
      }

      if (failures.nonEmpty) {
        Left[NonEmptyList[Throwable], (Seq[Throwable], PValue)](failures.reduce(_ |+| _))
      } else {
        Right[NonEmptyList[Throwable], (Seq[Throwable], PValue)]((warnings, PValue.fromMap(resSuccess.toMap)))
      }
    })(s => o => o)
  }

  def apply(p: PValue): NonEmptyList[Throwable] Either (Seq[Throwable], PValue) = validation.get(p)
}

object ProjectionW {
  def apply(validator: (Symbol, PValidationNelW)*): ProjectionW = ProjectionW(validator)

  implicit def projectionToValidation(p: ProjectionW): PValidationNelW = p.validation
}
