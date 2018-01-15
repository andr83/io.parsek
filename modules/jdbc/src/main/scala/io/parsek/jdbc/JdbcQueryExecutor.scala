package io.parsek.jdbc

import java.sql.{Connection, ResultSet}

import io.parsek.PValue
import io.parsek.PValue.PMap
import io.parsek.jdbc.implicits._
import io.parsek.types._
import resource._

/**
  * Default implementation of QueryExecutor
  *
  * @author Andrei Tupitcyn
  */
class JdbcQueryExecutor(
  val connection: Connection,
  val config: JdbcConfig = JdbcConfig()
) extends QueryExecutor {

  override def executeQuery[A](query: Query)(f: (ResultSet) => A): A = {
    val res = for {
      stmt <- managed(connection.prepareStatement(query.sql))
    } yield {
      var i = 1
      val it = query.params.iterator
      while (it.hasNext) {
        i = it.next().bind(stmt, i)
      }
      f(stmt.executeQuery())
    }
    res.acquireAndGet(identity)
  }

  override def execute(query: Query): Boolean = {
    val res = for {
      stmt <- managed(connection.prepareStatement(query.sql))
    } yield {
      var i = 1
      val it = query.params.iterator
      while (it.hasNext) {
        i = it.next().bind(stmt, i)
      }
      stmt.execute()
    }
    res.acquireAndGet(identity)
  }

  override def executeUpdate(query: Query): Int = {
    val res = for {
      stmt <- managed(connection.prepareStatement(query.sql))
    } yield {
      var i = 1
      val it = query.params.iterator
      while (it.hasNext) {
        i = it.next().bind(stmt, i)
      }
      stmt.executeUpdate()
    }
    res.acquireAndGet(identity)
  }

  override def buildInsertQuery(table: String, r: PMap): Query = {
    val nameConverter = config.nameConverter
    val schema = tableSchema(table).fields.map(f => Symbol(nameConverter(f.name.name)) -> f).toMap

    val binders = r.value.foldLeft(Map.empty[String, PValueBinder]) {
      case (b, (key, _)) =>
        if (schema.contains(key)) {
          val f = schema(key)
          b + (f.name.name -> valueBinder(f.dataType))
        } else {
          throw new IllegalStateException(s"Can not build insert query for table $table: column $key")
        }
    }

    val sql =
      s"""
         |INSERT INTO $table (${binders.keys.mkString(",")}) VALUES (${binders.values.map(_ => " ? ").mkString(",")})
      """.stripMargin.trim

    Query(sql, binders.map {
      case (k, b) => b(r.value.getOrElse(Symbol(nameConverter(k)), PValue.Null))
    })
  }

  override def batchInsert(table: String, it: Iterable[PMap], scheme: PStructType): Unit = {
    val nameConverter = config.nameConverter
    val binders = scheme.fields.map(f => f.name -> (Symbol(nameConverter(f.name.name)) -> valueBinder(f.dataType))).toMap

    val sql =
      s"""
         |INSERT INTO $table (${binders.keys.map(_.name).mkString(",")}) VALUES (${binders.values.map(_ => " ? ").mkString(",")})
     """.stripMargin.trim

    executeBatch(Query(sql), it.map { case PMap(m) => binders.map { case (k, (a, b)) => b(m.getOrElse(a, PValue.Null)) } })
  }

  override def executeBatch(query: Query, batchParams: Iterable[Iterable[ParameterBinder]]): Array[Int] = {
    val res = for {
      stmt <- managed(connection.prepareStatement(query.sql))
    } yield {
      var res = Array.empty[Int]
      connection.setAutoCommit(false)

      val it = batchParams.iterator
      while (it.hasNext) {
        val paramsIt = it.next().iterator
        var i = 1
        while (paramsIt.hasNext) {
          i = paramsIt.next().bind(stmt, i)
        }
        stmt.addBatch()
      }
      res = stmt.executeBatch()
      connection.commit()
      res
    }
    res.acquireAndGet(identity)
  }
}

object JdbcQueryExecutor {
  def apply(connection: Connection): JdbcQueryExecutor =
    new JdbcQueryExecutor(connection)

  def apply(connection: Connection, nameConverter: NameConverter): JdbcQueryExecutor =
    new JdbcQueryExecutor(connection, JdbcConfig(nameConverter = nameConverter))
}