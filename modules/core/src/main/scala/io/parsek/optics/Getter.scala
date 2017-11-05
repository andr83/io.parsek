package io.parsek.optics

import io.parsek.PResult

/**
  * @author Andrei Tupitcyn
  */
trait Getter[S, A] {
  def get(s: S): PResult[A]
}
