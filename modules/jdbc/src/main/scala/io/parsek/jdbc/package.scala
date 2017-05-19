package io.parsek

import java.sql.PreparedStatement

/**
  * @author Andrei Tupitcyn
  */
package object jdbc {
  implicit class SqlHelper(val sc: StringContext) extends AnyVal {
    def sql(args: Any*): Query = Query(sc.parts.mkString(" ? "), args.map(ParameterBinder.apply))
  }

  type NameConverter = String => String
  type ParameterBinder = (PreparedStatement, Int) => Unit
  type ValueBinder[A] = A => ParameterBinder
  type PValueBinder = PValue => ParameterBinder

  val LowerCaseConverter: NameConverter = name => name.toLowerCase
  val UpperCaseConverter: NameConverter = name => name.toUpperCase
}
