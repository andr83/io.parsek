package io.parsek.jdbc

import java.sql.PreparedStatement

/**
  * @author Andrei Tupitcyn
  * Allow to bind value to `java.sql.PreparedStatement` by index
  */
trait ParameterBinder {
  def bind(stmt: PreparedStatement, index: Int): Int
}