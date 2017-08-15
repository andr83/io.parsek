package io.parsek.jdbc.generic

import java.sql.{Connection, ResultSet}
import resource._

import io.parsek.PValue
import io.parsek.PValue.PMap
import io.parsek.jdbc._
import io.parsek.jdbc.generic.implicits._
import io.parsek.types._

import scala.collection.mutable

/**
  * @author Andrei Tupitcyn
  */
class  JdbcQueryExecutor(
  val connection: Connection,
  val encoder: ResultSetEncoder = ResultSetEncoder()
) extends QueryExecutor {
  import JdbcQueryExecutor._

  override def executeQuery[A](query: Query)(f: (ResultSet) => A): A = {
    val res = for {
      stmt <- managed(connection.prepareStatement(query.sql))
    } yield {
      var i = 1
      val it = query.params.iterator
      while (it.hasNext) {
        it.next().bind(stmt, i)
        i += 1
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
        it.next().bind(stmt, i)
        i += 1
      }
      stmt.execute()
    }
    res.acquireAndGet(identity)
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
          paramsIt.next().bind(stmt, i)
          i += 1
        }
        stmt.addBatch()
      }
      res = stmt.executeBatch()
      connection.commit()
      res
    }
    res.acquireAndGet(identity)
  }

  override def insert(table: String, r: PMap): Int = {
    val columnRs = connection.getMetaData.getColumns(null, null, table, null)
    var binders = Map.empty[String, PValueBinder]
    val nameConverter = encoder.nameConverter
    val m = r.value
    while (columnRs.next()) {
      val name = columnRs.getString("COLUMN_NAME")
      if (m.contains(Symbol(nameConverter(name)))) {
        binders += name -> valueBinder(getPType(columnRs.getInt("DATA_TYPE")))
      }
    }
    columnRs.close()

    val sql =
      s"""
         |INSERT INTO $table (${binders.keys.mkString(",")}) VALUES (${binders.values.map(_ => " ? ").mkString(",")})
      """.stripMargin.trim
    executeUpdate(Query(sql, binders.map { case (k, b) => b(m.getOrElse(Symbol(nameConverter(k)), PValue.Null)) }))
  }

  override def executeUpdate(query: Query): Int = {
    val res = for {
      stmt <- managed(connection.prepareStatement(query.sql))
    } yield {
      var i = 1
      val it = query.params.iterator
      while (it.hasNext) {
        it.next().bind(stmt, i)
        i += 1
      }
      stmt.executeUpdate()
    }
    res.acquireAndGet(identity)
  }

  override def batchInsert(table: String, it: Iterable[PMap]): Unit = {
    val columnRs = connection.getMetaData.getColumns(null, null, table, null)
    val fields = mutable.ArrayBuffer.empty[PStructField]

    while (columnRs.next()) {
      fields += PStructField(
        name = Symbol(columnRs.getString("COLUMN_NAME")),
        dataType = getPType(columnRs.getInt("DATA_TYPE"))
      )
    }
    columnRs.close()

    batchInsert(table, it, PStructType(fields))
  }

  override def batchInsert(table: String, it: Iterable[PMap], scheme: PStructType): Unit = {
    val nameConverter = encoder.nameConverter
    val binders = scheme.fields.map(f=> f.name -> (Symbol(nameConverter(f.name.name)), valueBinder(f.dataType))).toMap

    val sql =
      s"""
         |INSERT INTO $table (${binders.keys.map(_.name).mkString(",")}) VALUES (${binders.values.map(_ => " ? ").mkString(",")})
     """.stripMargin.trim

    executeBatch(Query(sql), it.map{case PMap(m) => binders.map {case (k,(a, b)) => b(m.getOrElse(a, PValue.Null))}})
  }
}

object JdbcQueryExecutor {
  def apply(connection: Connection): JdbcQueryExecutor =
    new JdbcQueryExecutor(connection)

  def apply(connection: Connection, encoder: ResultSetEncoder): JdbcQueryExecutor =
    new JdbcQueryExecutor(connection, encoder)

  def apply(connection: Connection, nameConverter: NameConverter): JdbcQueryExecutor =
    new JdbcQueryExecutor(connection, ResultSetEncoder(nameConverter))

  def getPType(sqlType: Int): PType = sqlType match {
    case java.sql.Types.BIT | java.sql.Types.SMALLINT | java.sql.Types.TINYINT | java.sql.Types.INTEGER => PIntType
    case java.sql.Types.BIGINT => PLongType
    case java.sql.Types.FLOAT | java.sql.Types.DOUBLE | java.sql.Types.REAL | java.sql.Types.DECIMAL => PDoubleType
    case java.sql.Types.BOOLEAN => PBooleanType
    case java.sql.Types.TIMESTAMP | java.sql.Types.TIMESTAMP_WITH_TIMEZONE => PInstantType
    case java.sql.Types.DATE => PInstantType
    case java.sql.Types.BLOB => PBinaryType
    case java.sql.Types.ARRAY => PArrayType()
    case _ => PStringType
  }
}