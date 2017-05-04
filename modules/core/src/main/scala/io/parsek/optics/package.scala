package io.parsek

/**
  * @author andr83 
  *         created on 03.05.17
  */
package object optics {
  type Prism[S, A] = PPrism[S, S, A, A]
  type Validation[S, E, A] = PValidation[S, S, E, A, A]
}
