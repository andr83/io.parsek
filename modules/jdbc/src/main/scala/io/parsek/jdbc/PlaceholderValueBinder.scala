package io.parsek.jdbc

import java.sql.PreparedStatement

/**
  * @author Andrei Tupitcyn
  */
final class PlaceholderValueBinder(binder: ParameterBinder, val toSql: (String, Int)) extends ParameterBinder {
  def bind(stmt: PreparedStatement, index: Int): Int = binder.bind(stmt, index)
}
