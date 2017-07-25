package io.parsek

import cats.data.NonEmptyList

/**
  * @author andr83 
  *         created on 03.05.17
  */
package object optics {
  type Prism[S, A] = PrismS[S, S, A, A]
  type Validation[S, E, A] = ValidationS[S, S, E, A, A]
  type PValidation[A] = Validation[PValue, Throwable, A]
  type PValidationNel = Validation[PValue, NonEmptyList[Throwable], PValue]
  type PValidationNelW = Validation[PValue, NonEmptyList[Throwable], (Seq[Throwable], PValue)]
}
