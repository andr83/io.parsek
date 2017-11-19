package io.parsek

/**
  * @author andr83 
  *         created on 03.05.17
  */
package object optics {
  type Lens[S, A] = PLens[S, S, A, A]
  type Traversal[S, A] = PTraversal[S, S, A, A]
}
