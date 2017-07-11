package io.parsek

/**
  * @author Andrei Tupitcyn
  */
package object jdbc {
  implicit class SqlHelper(val sc: StringContext) extends AnyVal {
    def sql(args: Any*): Query = Query(sc.parts.mkString(" ? "), args.map(ParameterBinder.apply))
  }

  type NameConverter = String => String

  val LowerCaseConverter: NameConverter = name => name.toLowerCase
  val UpperCaseConverter: NameConverter = name => name.toUpperCase
}
