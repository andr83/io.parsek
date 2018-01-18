package io.parsek.jdbc

import java.sql.JDBCType

import io.parsek.Decoder

/**
  * @author Andrei Tupitcyn
  * Describe conversion between A type and sql types
**/
trait ParameterTypeMeta[A] {
  /**
    * Name of SQL type
    * see java.sql.Types
    */
  def sqlType: String

  /**
    * JDBC type
    * see java.sql.Types
    */
  def jdbcType: Int

  def decoder: Decoder[A]
}

object ParameterTypeMeta {
  def apply[A](jdbcTyp: Int)(implicit _decoder: Decoder[A] = null): ParameterTypeMeta[A] = new ParameterTypeMeta[A] {
    override def sqlType: String = JDBCType.valueOf(jdbcType).getName

    override def jdbcType: Int = jdbcTyp

    override def decoder: Decoder[A] = if (_decoder == null) {
      throw new IllegalStateException(s"Can not find implicit for sql type $sqlType")
    } else decoder
  }
}