package io.parsek.jdbc

import java.sql.{ResultSet, ResultSetMetaData}
import java.time.Instant

import io.parsek.{Encoder, PValue}

/**
  * @author Andrei Tupitcyn
  */
class ResultSetEncoder(val nameConverter: NameConverter) {
  self =>
  def encode(meta: ResultSetMetaData): Encoder[ResultSet] = new Encoder[ResultSet] {
    private val columnCount = meta.getColumnCount
    private val columnNames = (1 to columnCount) map (i => Symbol(nameConverter(meta.getColumnLabel(i))))
    private val decoders = (1 to columnCount) map (self.encode(_, meta))

    override def apply(rs: ResultSet): PValue = {
      PValue.fromMap((0 until columnCount).map(i => columnNames(i) -> decoders(i)(rs)).toMap)
    }
  }

  def encode(i: Int, meta: ResultSetMetaData) = new Encoder[ResultSet] {
    private val _decoder = meta.getColumnType(i) match {
      case java.sql.Types.ARRAY => rs: ResultSet =>
        val arr = rs.getArray(i).asInstanceOf[Array[String]]
        PValue.fromValues(arr.map(PValue.fromString))
      case java.sql.Types.BIGINT => rs: ResultSet =>
        PValue.fromLong(rs.getLong(i))
      case java.sql.Types.BIT | java.sql.Types.SMALLINT | java.sql.Types.TINYINT | java.sql.Types.INTEGER => rs: ResultSet =>
        PValue.fromInt(rs.getInt(i))
      case java.sql.Types.FLOAT | java.sql.Types.DOUBLE | java.sql.Types.REAL | java.sql.Types.DECIMAL => rs: ResultSet =>
        PValue.fromDouble(rs.getDouble(i))
      case java.sql.Types.BOOLEAN => rs: ResultSet =>
        PValue.fromBoolean(rs.getBoolean(i))
      case java.sql.Types.TIMESTAMP | java.sql.Types.TIMESTAMP_WITH_TIMEZONE => rs: ResultSet =>
        PValue.fromInstant(rs.getTimestamp(i).toInstant)
      case java.sql.Types.DATE => rs: ResultSet =>
        PValue.fromInstant(Instant.ofEpochMilli(rs.getDate(i).getTime))
      case java.sql.Types.BLOB => rs: ResultSet =>
        val blob = rs.getBlob(i)
        PValue.fromBytes(blob.getBytes(0, blob.length().toInt))
      case _ => rs: ResultSet =>
        PValue.fromString(rs.getString(i))
    }

    override def apply(rs: ResultSet): PValue = _decoder(rs)
  }
}

object ResultSetEncoder {
  def apply(): ResultSetEncoder = new ResultSetEncoder(identity)
  def apply(nameConverter: NameConverter): ResultSetEncoder = new ResultSetEncoder(nameConverter)
}
