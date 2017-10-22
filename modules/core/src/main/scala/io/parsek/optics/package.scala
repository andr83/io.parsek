package io.parsek

import cats.data.NonEmptyList

/**
  * @author andr83 
  *         created on 03.05.17
  */
package object optics {
  type Prism[S, A] = PPrism[S, S, A, A]
  type Lens[S, A] = PLens[S, S, A, A]
}
