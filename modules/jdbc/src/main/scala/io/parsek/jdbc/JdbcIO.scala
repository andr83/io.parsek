package io.parsek.jdbc

import io.parsek.PValue.PMap
import io.parsek.syntax.traversable._
import io.parsek.{Encoder, PResult}

abstract class JdbcIO[A] {
  self =>

  def map[B](fa: A => B): JdbcIO[B] = new JdbcIO[B] {
    override def run(implicit qe: QueryExecutor): PResult[B] = self.run.map(fa)
  }

  def flatMap[B](fa: A => JdbcIO[B]): JdbcIO[B] = new JdbcIO[B] {
    override def run(implicit qe: QueryExecutor): PResult[B] = self.run.flatMap(fa(_).run)
  }

  def run(implicit qe: QueryExecutor): PResult[A]

  def unsafeRun(implicit qe: QueryExecutor): A = run.unsafe
}

object JdbcIO {
  def insert[A: Encoder](table: String, record: A): InsertIO[A] = InsertIO(table, record)

  def batchInsert[A: Encoder](table: String, records: Iterable[A]): BatchInsertIO[A] = BatchInsertIO(table, records)
}

/**
  * @author Andrei Tupitcyn
  */
case class QueryIO[A](private val query: Query, private val resultReader: ResultReader[A]) extends JdbcIO[A] {
  def run(implicit qe: QueryExecutor): PResult[A] = qe.executeQuery(query)(resultReader.read(_, qe.config))
}

case class ExecuteIO(private val query: Query) extends JdbcIO[Boolean] {
  def run(implicit qe: QueryExecutor): PResult[Boolean] = PResult.catchNonFatal(qe.execute(query))

  override def unsafeRun(implicit qe: QueryExecutor): Boolean = qe.execute(query)
}

case class UpdateIO(private val query: Query) extends JdbcIO[Int] {
  def run(implicit qe: QueryExecutor): PResult[Int] = PResult.catchNonFatal(qe.executeUpdate(query))

  override def unsafeRun(implicit qe: QueryExecutor): Int = qe.executeUpdate(query)
}

case class InsertIO[A: Encoder](table: String, record: A) extends JdbcIO[Int] {
  def run(implicit qe: QueryExecutor): PResult[Int] = implicitly[Encoder[A]].apply(record) match {
    case pm: PMap =>
      PResult catchNonFatal {
        val iq = qe.buildInsertQuery(table, pm)
        qe.executeUpdate(iq)
      }
    case other => PResult.invalid(new IllegalStateException(s"Can not create insert query for $other. Maybe primitive?"))

  }
}

case class BatchInsertIO[A: Encoder](table: String, it: Iterable[A]) extends JdbcIO[Unit] {
  private val encoder = implicitly[Encoder[A]]

  def run(implicit qe: QueryExecutor): PResult[Unit] = {
    it
      .map(encoder.apply).map {
      case pm: PMap => PResult.valid(pm)
      case other => PResult.invalid(new IllegalStateException(s"Can not create batch query for $other. Maybe primitive?"))
    }
      .toList
      .toPResult
      .flatMap(data => PResult.catchNonFatal(qe.batchInsert(table, data)))
  }
}
