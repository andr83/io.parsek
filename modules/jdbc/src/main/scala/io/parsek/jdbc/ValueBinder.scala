package io.parsek.jdbc

import java.sql.PreparedStatement

import io.parsek.{Decoder, PValue}
import io.parsek.jdbc.instances.parameterBinder.nullParameterBinder

/**
  * @author Andrei Tupitcyn
  *
  * Bind exactly value to `java.sql.PreparedStatement`
  */
trait ValueBinder[A] extends (A => ParameterBinder)

object ValueBinder {
  @inline def pure[A: ParameterTypeMeta](f: (PreparedStatement, Int, A) => Int): ValueBinder[A] = {
    new ValueBinder[A] {
      override def apply(a: A): ParameterBinder =
        if (a == null) nullParameterBinder[A] else {
          new ParameterBinder {
            def bind(stmt: PreparedStatement, index: Int): Int = f(stmt, index, a)
          }
        }
    }
  }

  @inline def wrap[A: Decoder](binder: ValueBinder[A]): ValueBinder[PValue] = {
    val decoder = implicitly[Decoder[A]]
    new ValueBinder[PValue] {
      override def apply(v: PValue): ParameterBinder = binder.apply(decoder.unsafe(v))
    }
  }
}
