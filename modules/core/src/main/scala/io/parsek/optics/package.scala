package io.parsek

/**
  * @author andr83 
  *         created on 03.05.17
  */
package object optics {
  type Prism[S, A] = PPrism[S, S, A, A]
  type Optional[S, A] = POptional[S, S, A, A]
}
