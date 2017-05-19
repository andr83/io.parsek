package io.parsek.jdbc

import java.sql.{Connection, ResultSet}

import io.parsek.PValue
import io.parsek.PValue.PMap

/**
  * @author Andrei Tupitcyn
  */
abstract class QueryExecutor {
  val connection: Connection
  val encoder: ResultSetEncoder

  def executeQuery[A](query: Query)(f: ResultSet => A): A

  def executeUpdate(query: Query): Int

  def execute(query: Query): Boolean

  def executeBatch(query: Query, batchParams: Iterable[Iterable[ParameterBinder]]): Array[Int]

  def insert(table: String, value: PMap): Int
  def batchInsert(it: Iterable[PValue]): Unit
}
