package io.parsek.jdbc.instances

import java.sql.PreparedStatement

import io.parsek.jdbc.{ParameterBinder, ParameterTypeMeta}

/**
  * @author Andrei Tupitcyn
  */
trait ParameterBinderInstances {
  @inline def nullParameterBinder[A: ParameterTypeMeta]: ParameterBinder = {
    val sqlType = implicitly[ParameterTypeMeta[A]].jdbcType
    new ParameterBinder {
      def bind(stmt: PreparedStatement, index: Int): Int = {
        stmt.setNull(index, sqlType)
        index + 1
      }
    }
  }
}
