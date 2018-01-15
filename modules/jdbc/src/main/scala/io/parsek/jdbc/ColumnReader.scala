package io.parsek.jdbc

import java.sql.ResultSet
import io.parsek.PResult

/**
  * @author Andrei Tupitcyn
  */
trait ColumnReader[A] {
  def read(rs: ResultSet, index: Int): PResult[A]

  def read(rs: ResultSet, name: String): PResult[A]
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