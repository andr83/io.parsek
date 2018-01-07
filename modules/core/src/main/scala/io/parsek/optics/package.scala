package io.parsek

/**
  * @author Andrei Tupitcyn
  */
package object optics {
  type Lens[S, A] = PLens[S, S, A, A]
  type Traversal[S, A] = PTraversal[S, S, A, A]
}
