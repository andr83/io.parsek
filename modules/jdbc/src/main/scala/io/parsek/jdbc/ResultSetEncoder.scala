package io.parsek.jdbc

import java.sql.{ResultSet, ResultSetMetaData}
import java.time.Instant

import ResultSetEncoder._
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
    private val _decoder: ResultSet => PValue = resultSet2PValue(meta, i)

    override def apply(rs: ResultSet): PValue = _decoder(rs)
  }
}

object ResultSetEncoder {
  def apply(): ResultSetEncoder = new ResultSetEncoder(identity)
  def apply(nameConverter: NameConverter): ResultSetEncoder = new ResultSetEncoder(nameConverter)

  private def resultSet2PValue(meta: ResultSetMetaData, i: Int): ResultSet => PValue = meta.getColumnType(i) match {
    case java.sql.Types.ARRAY => rs: ResultSet =>
      if (rs.getObject(i) == null) {
        PValue.Null
      } else {
        val arr = getArray(rs.getArray(i).getArray.asInstanceOf[Any])
        PValue.fromValues(arr.map(object2PValue))
//        val ars = rs.getArray(i).getResultSet()
//        val decoder = resultSet2PValue(ars.getMetaData, 2)
//        var res = Seq.empty[PValue]
//        while (ars.next()) {
//          res = res :+ decoder(ars)
//        }
//        PValue.fromValues(res)
      }
    case java.sql.Types.BIGINT => rs: ResultSet =>
      if (rs.getObject(i) == null) {
        PValue.Null
      } else {
        PValue.fromLong(rs.getLong(i))
      }
    case java.sql.Types.BIT | java.sql.Types.SMALLINT | java.sql.Types.TINYINT | java.sql.Types.INTEGER => rs: ResultSet =>
      if (rs.getObject(i) == null) {
        PValue.Null
      } else {
        PValue.fromInt(rs.getInt(i))
      }
    case java.sql.Types.FLOAT | java.sql.Types.DOUBLE | java.sql.Types.REAL | java.sql.Types.DECIMAL => rs: ResultSet =>
      if (rs.getObject(i) == null) {
        PValue.Null
      } else {
        PValue.fromDouble(rs.getDouble(i))
      }
    case java.sql.Types.BOOLEAN => rs: ResultSet =>
      if (rs.getObject(i) == null) {
        PValue.Null
      } else {
        PValue.fromBoolean(rs.getBoolean(i))
      }
    case java.sql.Types.TIMESTAMP | java.sql.Types.TIMESTAMP_WITH_TIMEZONE => rs: ResultSet =>
      if (rs.getObject(i) == null) {
        PValue.Null
      } else {
        PValue.fromInstant(rs.getTimestamp(i).toInstant)
      }
    case java.sql.Types.DATE => rs: ResultSet =>
      if (rs.getObject(i) == null) {
        PValue.Null
      } else {
        PValue.fromInstant(Instant.ofEpochMilli(rs.getDate(i).getTime))
      }
    case java.sql.Types.BLOB => rs: ResultSet =>
      if (rs.getObject(i) == null) {
        PValue.Null
      } else {
        val blob = rs.getBlob(i)
        PValue.fromBytes(blob.getBytes(0, blob.length().toInt))
      }
    case java.sql.Types.NULL => rs: ResultSet =>
      object2PValue(rs.getObject(i))
    case _ => rs: ResultSet =>
      if (rs.getObject(i) == null) {
        PValue.Null
      } else {
        PValue.fromString(rs.getString(i))
      }
  }

  def object2PValue(obj: Any): PValue = obj match {
    case x: java.lang.Boolean => PValue.fromBoolean(x)
    case x: java.lang.Integer => PValue.fromInt(x)
    case x: java.lang.Byte => PValue.fromInt(x.intValue())
    case x: java.lang.Short => PValue.fromInt(x.intValue())
    case x: java.lang.Long => PValue.fromLong(x)
    case x: java.lang.Number => PValue.fromDouble(x.doubleValue())
    case x: java.sql.Timestamp => PValue.fromInstant(x.toInstant)
    case null => PValue.Null
    case x => PValue.fromString(x.toString)
  }

  private def getArray(value: Any): Array[AnyRef] = {
    value match {
      case objArr: Array[AnyRef] => objArr
      case _ =>
        val arrlength = java.lang.reflect.Array.getLength(value)
        (0 until arrlength).map(i=> java.lang.reflect.Array.get(value, i)).toArray[AnyRef]
    }
  }
}
