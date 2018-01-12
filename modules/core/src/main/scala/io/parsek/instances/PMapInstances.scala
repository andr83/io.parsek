package io.parsek.instances

import io.parsek.PValue
import io.parsek.algebra.Semigroup

/**
  * @author Andrei Tupitcyn
  */
trait PMapInstances {
  implicit val pmapSemigroup = new Semigroup[Map[Symbol, PValue]]  {

    def empty: Map[Symbol, PValue] = Map.empty

    def combine(xs: Map[Symbol, PValue], ys: Map[Symbol, PValue]): Map[Symbol, PValue] =
      if (xs.size <= ys.size) {
        xs.foldLeft(ys) { case (my, (k, x)) =>
          my.updated(k, my.getOrElse(k, x))
        }
      } else {
        ys.foldLeft(xs) { case (mx, (k, y)) =>
          mx.updated(k, mx.getOrElse(k, y))
        }
      }
  }
}

object PMapInstances extends PMapInstances
