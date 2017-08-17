package io.parsek

import java.sql.{JDBCType, PreparedStatement}

import scala.language.implicitConversions

/**
  * @author Andrei Tupitcyn
  */
package object jdbc {

  trait ToSql[A] extends (A => (String, Int))

  trait ParameterBinder {
    def bind(stmt: PreparedStatement, index: Int): Int
  }

  trait ValueBinder[A] extends (A => ParameterBinder)

  type PValueBinder = ValueBinder[PValue]

  object ValueBinder {
    @inline def nullParameterBinder[A : ParameterTypeMeta]: ParameterBinder = {
      val sqlType = implicitly[ParameterTypeMeta[A]].jdbcType
      new ParameterBinder {
        def bind(stmt: PreparedStatement, index: Int): Int = {
          stmt.setNull(index, sqlType)
          index + 1
        }
      }
    }

    @inline def pure[A : ParameterTypeMeta](f: (PreparedStatement, Int, A) => Int): ValueBinder[A] = {
      new ValueBinder[A] {
        override def apply(a: A): ParameterBinder =
          if (a == null) nullParameterBinder[A] else {
            new ParameterBinder {
              def bind(stmt: PreparedStatement, index: Int): Int = f(stmt, index, a)
            }
          }
      }
    }

    @inline def wrap[A : Decoder](binder: ValueBinder[A]): ValueBinder[PValue] = {
      val decoder = implicitly[Decoder[A]]
      new ValueBinder[PValue] {
        override def apply(v: PValue): ParameterBinder = binder.apply(decoder.unsafe(v))
      }
    }
  }

  /**
    * Describe conversion between A type and sql types
    * @tparam A
    */
  trait ParameterTypeMeta[A] {
    /**
      * Name of SQL type
      * @see [[java.sql.Types]]
      */
    def sqlType: String

    /**
      * JDBC type
      * @see [[java.sql.Types]]
      */
    def jdbcType: Int

    def decoder: Decoder[A]
  }

  object ParameterTypeMeta {
    def apply[A](jdbcTyp: Int)(implicit decoder: Decoder[A] = null): ParameterTypeMeta[A] = new ParameterTypeMeta[A] {
      override def sqlType: String = JDBCType.valueOf(jdbcType).getName

      override def jdbcType: Int = jdbcTyp

      override def decoder: Decoder[A] = if (decoder == null) {
        throw new IllegalStateException(s"Can not find implicit for sql type $sqlType")
      } else decoder
    }
  }

  final class PlaceholderValueBinder(binder: ParameterBinder, val toSql: (String, Int)) extends ParameterBinder {
    def bind(stmt: PreparedStatement, index: Int): Int = binder.bind(stmt, index)
  }

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


  type NameConverter = String => String

  val LowerCaseConverter: NameConverter = name => name.toLowerCase
  val UpperCaseConverter: NameConverter = name => name.toUpperCase
}
