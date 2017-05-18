package io.parsek.optics

import io.parsek.PValue

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * @author Andrei Tupitcyn
  */
case class Projection(validators: Iterable[(Symbol, Validation[PValue, Throwable, PValue])]) {
  private val failures = mutable.ArrayBuffer.empty[Throwable]

  val validation: Validation[PValue, Throwable, PValue] = {
    Validation[PValue, Throwable, PValue](rec => {
      val resSuccess = mutable.Map.empty[Symbol, PValue]

      val it = validators.iterator
      while (it.hasNext) {
        val (toKey, validator) = it.next()
        validator.get(rec) match {
          case Right(v) =>
            resSuccess += toKey -> v
          case Left(err) =>
            failures += err
        }
      }

      if (failures.nonEmpty) {
        Left[Throwable, PValue](failures.last)
      } else {
        Right[Throwable, PValue](PValue.fromMap(resSuccess.toMap))
      }
    })(s => o => o)
  }

  def apply(p: PValue): Throwable Either PValue = validation.get(p)
}

object Projection {
  def apply(validator: (Symbol, Validation[PValue, Throwable, PValue])*): Projection = Projection(validator)

  implicit def projectionToValidation(p: Projection): Validation[PValue, Throwable, PValue] = p.validation
}
