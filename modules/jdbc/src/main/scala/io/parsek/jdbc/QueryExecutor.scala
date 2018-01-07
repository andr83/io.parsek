package io.parsek.jdbc

import java.sql.{Connection, ResultSet}

import io.parsek.PValue.PMap
import io.parsek.types._

import scala.collection.mutable

/**
  * @author Andrei Tupitcyn
  */
abstract class QueryExecutor {
  val connection: Connection
  val config: JdbcConfig

  /** Execute query and return result */
  def executeQuery[A](query: Query)(f: ResultSet => A): A

  /** Execute DML query and return count of affected records */
  def executeUpdate(query: Query): Int

  /** Execute query and return  */
  def execute(query: Query): Boolean

  /** Execute query in batch */
  def executeBatch(query: Query, batchParams: Iterable[Iterable[ParameterBinder]]): Array[Int]

  /** Insert record to the table */
  def buildInsertQuery(table: String, record: PMap): Query

  /** Batch insert of records to the table with schema */
  def batchInsert(table: String, it: Iterable[PMap], scheme: PStructType): Unit

  /** Batch insert of records to the table */
  def batchInsert(table: String, it: Iterable[PMap]): Unit = batchInsert(table, it, tableSchema(table))

  def tableSchema(table: String): PStructType = {
    val columnRs = connection.getMetaData.getColumns(null, null, table, null)
    val fields = mutable.ArrayBuffer.empty[PStructField]

    while (columnRs.next()) {
      fields += PStructField(
        name = Symbol(columnRs.getString("COLUMN_NAME")),
        dataType = getPType(columnRs.getInt("DATA_TYPE"))
      )
    }
    columnRs.close()
    PStructType(fields)
  }

  def getPType(sqlType: Int): PType = sqlType match {
    case java.sql.Types.BIT => PIntType
    case java.sql.Types.SMALLINT => PIntType
    case java.sql.Types.TINYINT => PIntType
    case java.sql.Types.INTEGER => PIntType
    case java.sql.Types.BIGINT => PLongType
    case java.sql.Types.FLOAT => PDoubleType
    case java.sql.Types.DOUBLE => PDoubleType
    case java.sql.Types.REAL => PDoubleType
    case java.sql.Types.DECIMAL => PDoubleType
    case java.sql.Types.BOOLEAN => PBooleanType
    case java.sql.Types.TIMESTAMP => PInstantType
    case java.sql.Types.TIMESTAMP_WITH_TIMEZONE => PInstantType
    case java.sql.Types.DATE => PDateType
    case java.sql.Types.BLOB => PBinaryType
    case java.sql.Types.ARRAY => PArrayType()
    case _ => PStringType
  }
}
