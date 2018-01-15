package io.parsek.jdbc

import java.sql.ResultSet
import io.parsek.PResult

/**
  * @author Andrei Tupitcyn
  */
trait ResultReader[A] {
  def read(rs: ResultSet, config: JdbcConfig): PResult[A]
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
