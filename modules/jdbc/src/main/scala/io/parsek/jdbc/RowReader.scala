package io.parsek.jdbc

import java.sql.ResultSet

import io.parsek.PResult

/**
  * @author Andrei Tupitcyn
  */
trait RowReader[A] {
  def read(rs: ResultSet): PResult[A]
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
