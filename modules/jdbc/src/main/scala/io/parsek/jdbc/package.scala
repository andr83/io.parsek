package io.parsek

import java.sql.{JDBCType, PreparedStatement, ResultSet}

import scala.language.{higherKinds, implicitConversions}

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

  trait ResultReader[A] {
    def read(rs: ResultSet, config: JdbcConfig): PResult[A]
  }

  trait RowReader[A] {
    def read(rs: ResultSet): PResult[A]
  }

  trait ColumnReader[A] {
    def read(rs: ResultSet, index: Int): PResult[A]

    def read(rs: ResultSet, name: String): PResult[A]
  }

  /**
    * Represent how type `A` should be converted to sql fragment with placeholder.
    * Return sql fragment and count of placeholders
    **/
  trait ToSql[A] extends (A => (String, Int))

  /** Allow to bind value to `java.sql.PreparedStatement` by index */
  trait ParameterBinder {
    def bind(stmt: PreparedStatement, index: Int): Int
  }

  /** Bind exactly value to `java.sql.PreparedStatement` */
  trait ValueBinder[A] extends (A => ParameterBinder)

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

  final class PlaceholderValueBinder(binder: ParameterBinder, val toSql: (String, Int)) extends ParameterBinder {
    def bind(stmt: PreparedStatement, index: Int): Int = binder.bind(stmt, index)
  }

  case class JdbcConfig(
    nameConverter: NameConverter = IdNameConverter
  )

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

  object ResultReader {
    def single[A](columnIndex: Int = 1)(implicit columnReader: ColumnReader[A]): ResultReader[A] = new ResultReader[A] {
      override def read(rs: ResultSet, config: JdbcConfig): PResult[A] =
        if (rs.next()) {
          RowReader.column(columnIndex).read(rs)
        } else PResult.invalid(new IllegalStateException("Can not iterate over ResultSet"))
    }

    def single[A](columnName: String)(implicit columnReader: ColumnReader[A]): ResultReader[A] = new ResultReader[A] {
      override def read(rs: ResultSet, config: JdbcConfig): PResult[A] =
        if (rs.next()) {
          RowReader.column(columnName).read(rs)
        } else PResult.invalid(new IllegalStateException("Can not iterate over ResultSet"))
    }
  }

  object RowReader {
    def pure[A](f: ResultSet => PResult[A]): RowReader[A] = new RowReader[A] {
      override def read(rs: ResultSet): PResult[A] = f(rs)
    }

    def column[A](columnIndex: Int = 1)(implicit columnReader: ColumnReader[A]): RowReader[A] = new RowReader[A] {
      override def read(rs: ResultSet): PResult[A] = columnReader.read(rs, columnIndex)
    }

    def column[A](name: String)(implicit columnReader: ColumnReader[A]): RowReader[A] = new RowReader[A] {
      override def read(rs: ResultSet): PResult[A] = columnReader.read(rs, name)
    }
  }

  object ColumnReader {
    def pure[A](
      indexReader: (ResultSet, Int) => A,
      nameReader: (ResultSet, String) => A
    ): ColumnReader[A] = new ColumnReader[A] {
      override def read(rs: ResultSet, index: Int): PResult[A] = PResult.catchNonFatal(indexReader(rs, index))

      override def read(rs: ResultSet, name: String): PResult[A] = PResult.catchNonFatal(nameReader(rs, name))
    }
  }

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

    @inline def nullParameterBinder[A: ParameterTypeMeta]: ParameterBinder = {
      val sqlType = implicitly[ParameterTypeMeta[A]].jdbcType
      new ParameterBinder {
        def bind(stmt: PreparedStatement, index: Int): Int = {
          stmt.setNull(index, sqlType)
          index + 1
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

  object ParameterTypeMeta {
    def apply[A](jdbcTyp: Int)(implicit decoder: Decoder[A] = null): ParameterTypeMeta[A] = new ParameterTypeMeta[A] {
      override def sqlType: String = JDBCType.valueOf(jdbcType).getName

      override def jdbcType: Int = jdbcTyp

      override def decoder: Decoder[A] = if (decoder == null) {
        throw new IllegalStateException(s"Can not find implicit for sql type $sqlType")
      } else decoder
    }
  }
}
