package io.parsek

/**
  * Jdbc module add simple jdbc layer for ETL application with selecting and batch operation support
  *
  * @author Andrei Tupitcyn
  */
package object jdbc {

  type PValueBinder = ValueBinder[PValue]
  type NameConverter = String => String
  val IdNameConverter: NameConverter = name => name
  val LowerCaseConverter: NameConverter = name => name.toLowerCase
  val UpperCaseConverter: NameConverter = name => name.toUpperCase

  implicit class SqlHelper(val sc: StringContext) extends AnyVal {
    def sql(binders: PlaceholderValueBinder*): Query = {
      val (_, builder) = sc.parts.foldLeft(0 -> StringBuilder.newBuilder) {
        case ((index, sb), part) =>
          if (index < binders.length) {
            val (fragment, _) = binders(index).toSql
            sb
              .append(part)
              .append(fragment)
          } else {
            sb.append(part)
          }
          index + 1 -> sb
      }
      Query(builder.toString,binders)
    }
  }

  implicit def convert2ToSql[A: ToSql](a: A): ToSql[A] = implicitly[ToSql[A]]

  implicit def convert2ValueBinder[A: ValueBinder](a: A): ValueBinder[A] = implicitly[ValueBinder[A]]

  implicit def convert2PlaceholderValueBinder[A: ToSql : ValueBinder](a: A): PlaceholderValueBinder =
    new PlaceholderValueBinder(implicitly[ValueBinder[A]].apply(a), implicitly[ToSql[A]].apply(a))

  implicit def convert2OptPlaceholderValueBinder[A: ToSql : ValueBinder](o: Option[A]): Option[PlaceholderValueBinder] = o match {
    case Some(a) => Some(convert2PlaceholderValueBinder(a))
    case None => None
  }
}
