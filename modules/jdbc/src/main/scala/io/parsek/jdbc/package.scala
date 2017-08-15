package io.parsek

import java.sql.{JDBCType, PreparedStatement}

import io.parsek.PValue.PBoolean
import io.parsek.types.PType

import scala.language.implicitConversions

/**
  * @author Andrei Tupitcyn
  */
package object jdbc {

  trait ToSql[A] extends (A => (String, Int))

  object ToSql {
    implicit def defaultToSql[A] = new ToSql[A] {
      override def apply(a: A): (String, Int) = ("?", 1)
    }
  }

  trait ParameterBinder {
    def bind(stmt: PreparedStatement, index: Int): Unit
  }

  trait ValueBinder[A] extends (A => ParameterBinder)

  type PValueBinder = ValueBinder[PValue]

  object ValueBinder {
    @inline def nullParameterBinder[A : ParameterTypeMeta]: ParameterBinder = {
      val sqlType = implicitly[ParameterTypeMeta[A]].jdbcType
      new ParameterBinder {
        def bind(stmt: PreparedStatement, index: Int): Unit = stmt.setNull(index, sqlType)
      }
    }

    @inline def pure[A : ParameterTypeMeta](f: (PreparedStatement, Int, A) => Unit): ValueBinder[A] = {
      new ValueBinder[A] {
        override def apply(a: A): ParameterBinder =
          if (a == null) nullParameterBinder[A] else {
            new ParameterBinder {
              def bind(stmt: PreparedStatement, index: Int): Unit = f(stmt, index, a)
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

  final class PlaceholderValueBinder(binder: ParameterBinder, toSql: (String, Int)) extends ParameterBinder {
    def bind(stmt: PreparedStatement, index: Int): Unit = binder.bind(stmt, index)
  }

  implicit class SqlHelper(val sc: StringContext) extends AnyVal {
    def sql(args: PlaceholderValueBinder*): Query = Query(sc.parts.mkString(" ? "), args)
  }

  implicit def convert2ToSql[A: ToSql](a: A): ToSql[A] = implicitly[ToSql[A]]

  implicit def convert2ValueBinder[A: ValueBinder](a: A): ValueBinder[A] = implicitly[ValueBinder[A]]

  implicit def convert2PlaceholderValueBinder[A: ToSql : ValueBinder](a: A): PlaceholderValueBinder =
    new PlaceholderValueBinder(implicitly[ValueBinder[A]].apply(a), implicitly[ToSql[A]].apply(a))


  type NameConverter = String => String

  val LowerCaseConverter: NameConverter = name => name.toLowerCase
  val UpperCaseConverter: NameConverter = name => name.toUpperCase
}
