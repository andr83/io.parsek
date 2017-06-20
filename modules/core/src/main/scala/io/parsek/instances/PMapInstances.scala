package io.parsek.instances

import cats.kernel.Monoid
import io.parsek.PValue

/**
  * @author andr83 
  *         created on 20.06.17
  */
trait PMapInstances {
  implicit val pmapMonoid = new Monoid[Map[Symbol, PValue]]  {

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
