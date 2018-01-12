package io.parsek.jdbc.instances

import java.sql.ResultSet

import io.parsek.implicits._
import io.parsek.jdbc.{ColumnReader, JdbcConfig, JdbcUtils, ResultReader, RowReader}
import io.parsek._

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * @author Andrei Tupitcyn
  */
trait ResultReaderInstances {
  implicit def traversableResultReader[A, T[_] <: Traversable[_]](
    implicit rowReader: RowReader[A], cbf: CanBuildFrom[Nothing, A, T[A]]
  ) = new ResultReader[T[A]] {
    override def read(rs: ResultSet, config: JdbcConfig): PResult[T[A]] = {
      val cb = cbf()
      val errors = List.empty[Throwable]

      while (rs.next()) {
        rowReader.read(rs) match {
          case PSuccess(a, _) => cb += a
          case PError(nel) => errors ++ nel.toList
        }
      }
      if (errors.nonEmpty) {
        PResult.invalid(NonEmptyList.fromListUnsafe(errors))
      } else {
        PResult.valid(cb.result())
      }
    }
  }

  implicit def singleResultReader[A](implicit columnReader: ColumnReader[A]): ResultReader[A] = ResultReader.single[A]()

  implicit def pvalueTraversablResultReader[T[_] <: Traversable[_]](
    implicit cbf: CanBuildFrom[Nothing, PValue, T[PValue]]
  ): ResultReader[T[PValue]] = {
    new ResultReader[T[PValue]] {
      override def read(rs: ResultSet, config: JdbcConfig): PResult[T[PValue]] = {
        val meta = rs.getMetaData
        val columnCount = meta.getColumnCount
        val columnNames = (1 to columnCount) map (i => Symbol(config.nameConverter(meta.getColumnLabel(i))))
        val columns = (1 to columnCount) map (i => JdbcUtils.getPValueColumnReader(meta.getColumnType(i)))
        val cb = cbf()

        while (rs.next()) {
          val row = (0 until columnCount).map(i => columns(i).read(rs, i + 1).map(columnNames(i) -> _))
            .toPResult
            .map(PValue.fromFields)
          row match {
            case PSuccess(v, _) => cb += v
            case e: PError => return e
          }
        }
        PResult.valid(cb.result())
      }
    }
  }

  implicit val pvalueResultReader: ResultReader[PValue] = new ResultReader[PValue] {
    override def read(rs: ResultSet, config: JdbcConfig): PResult[PValue] =
      pvalueTraversablResultReader[Seq].read(rs, config).map(_.headOption.getOrElse(PValue.Null))
  }
}
