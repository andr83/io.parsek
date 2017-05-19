package io.parsek.jdbc.generic

import java.sql.{Connection, ResultSet}

import io.parsek.PValue
import io.parsek.PValue.PMap
import io.parsek.jdbc._

/**
  * @author Andrei Tupitcyn
  */
class JdbcQueryExecutor(
  val connection: Connection,
  val encoder: ResultSetEncoder = ResultSetEncoder()
) extends QueryExecutor {
  override def executeQuery[A](query: Query)(f: (ResultSet) => A): A = {
    val stmt = connection.prepareStatement(query.sql)
    var i = 1
    val it = query.params.iterator
    while (it.hasNext) {
      it.next()(stmt, i)
      i += 1
    }
    f(stmt.executeQuery())
  }

  override def execute(query: Query): Boolean = {
    val stmt = connection.prepareStatement(query.sql)
    var i = 1
    val it = query.params.iterator
    while (it.hasNext) {
      it.next()(stmt, i)
      i += 1
    }
    stmt.execute()
  }

  override def executeBatch(query: Query, batchParams: Iterable[Iterable[ParameterBinder]]): Array[Int] = {
    val stmt = connection.prepareStatement(query.sql)
    val isAutoCommit = connection.getAutoCommit
    var res = Array.empty[Int]
    connection.setAutoCommit(false)

    try {

      val it = batchParams.iterator
      while (it.hasNext) {
        val paramsIt = it.next().iterator
        var i = 1
        while (paramsIt.hasNext) {
          paramsIt.next()(stmt, i)
          i += 1
        }
        stmt.addBatch()
      }
      res = stmt.executeBatch()
      connection.commit()
    } finally {
      connection.rollback()
      connection.setAutoCommit(isAutoCommit)
    }
    res
  }

  override def insert(table: String, value: PMap): Int = {
    val columnRs = connection.getMetaData.getColumns(null, null, table, null)
    var binders = Map.empty[String, PValueBinder]
    val nameConverter = encoder.nameConverter
    val m = value.value
    while (columnRs.next()) {
      val name = columnRs.getString("COLUMN_NAME")
      if (m.contains(Symbol(nameConverter(name)))) {
        binders += name -> PValueBinder(columnRs.getInt("DATA_TYPE"))
      }
    }

    val sql =
      s"""
         |INSERT INTO $table (${binders.keys.mkString(",")}) VALUES (${binders.values.map(_ => " ? ").mkString(",")})
     """.stripMargin.trim

    executeUpdate(Query(sql, binders.map { case (k, b) => b(value.value.getOrElse(Symbol(nameConverter(k)), PValue.Null)) }))
  }

  override def executeUpdate(query: Query): Int = {
    val stmt = connection.prepareStatement(query.sql)
    var i = 1
    val it = query.params.iterator
    while (it.hasNext) {
      it.next()(stmt, i)
      i += 1
    }
    stmt.executeUpdate()
  }

  override def batchInsert(it: Iterable[PValue]): Unit = ???
}

object JdbcQueryExecutor {
  def apply(connection: Connection): JdbcQueryExecutor =
    new JdbcQueryExecutor(connection)

  def apply(connection: Connection, encoder: ResultSetEncoder): JdbcQueryExecutor =
    new JdbcQueryExecutor(connection, encoder)

  def apply(connection: Connection, nameConverter: NameConverter): JdbcQueryExecutor =
    new JdbcQueryExecutor(connection, ResultSetEncoder(nameConverter))
}