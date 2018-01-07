package io.parsek.jdbc

import io.parsek.PValue
import io.parsek.PValue.PArray

import scala.util.Try

/**
  * @author Andrei Tupitcyn
  */
object JdbcUtils {
  def getPValueColumnReader(sqlType: Int): ColumnReader[PValue] =
    if (Seq(java.sql.Types.BIT, java.sql.Types.SMALLINT, java.sql.Types.TINYINT, java.sql.Types.INTEGER).contains(sqlType)) {
      ColumnReader.pure[PValue](
        (rs, i) => getPValue(rs.getInt(i), PValue.fromInt),
        (rs, name) => getPValue(rs.getInt(name), PValue.fromInt)
      )
    } else if (sqlType == java.sql.Types.BIGINT) {
      ColumnReader.pure[PValue](
        (rs, i) => getPValue(rs.getLong(i), PValue.fromLong),
        (rs, name) => getPValue(rs.getLong(name), PValue.fromLong)
      )
    } else if (Seq(java.sql.Types.FLOAT, java.sql.Types.DOUBLE, java.sql.Types.REAL, java.sql.Types.DECIMAL).contains(sqlType)) {
      ColumnReader.pure[PValue](
        (rs, i) => getPValue(rs.getDouble(i), PValue.fromDouble),
        (rs, name) => getPValue(rs.getDouble(name), PValue.fromDouble)
      )
    } else if (Seq(java.sql.Types.BOOLEAN, java.sql.Types.BIT, java.sql.Types.SMALLINT, java.sql.Types.TINYINT, java.sql.Types.INTEGER).contains(sqlType)) {
      ColumnReader.pure[PValue](
        (rs, i) => getPValue(rs.getBoolean(i), PValue.fromBoolean),
        (rs, name) => getPValue(rs.getBoolean(name), PValue.fromBoolean)
      )
    } else if (Seq(java.sql.Types.TIMESTAMP, java.sql.Types.TIMESTAMP_WITH_TIMEZONE).contains(sqlType)) {
      ColumnReader.pure[PValue](
        (rs, i) => getPValue(rs.getTimestamp(i).toInstant, PValue.fromInstant),
        (rs, name) => getPValue(rs.getTimestamp(name).toInstant, PValue.fromInstant)
      )
    } else if (sqlType == java.sql.Types.DATE) {
      ColumnReader.pure[PValue](
        (rs, i) => getPValue(rs.getDate(i).toLocalDate, PValue.fromLocalDate),
        (rs, name) => getPValue(rs.getDate(name).toLocalDate, PValue.fromLocalDate)
      )
    } else if (sqlType == java.sql.Types.BLOB) {
      ColumnReader.pure[PValue](
        (rs, i) => {
          val blob = rs.getBlob(i)
          PValue.fromBytes(blob.getBytes(0, blob.length().toInt))
        },
        (rs, name) => {
          val blob = rs.getBlob(name)
          PValue.fromBytes(blob.getBytes(0, blob.length().toInt))
        }
      )
    } else if (sqlType == java.sql.Types.ARRAY) {
      ColumnReader.pure[PValue](
        (rs, i) => {
          if (rs.getObject(i) == null) {
            PValue.Null
          } else {
            Try {
              val arr = getArray(rs.getArray(i).getArray.asInstanceOf[Any])
              PValue.fromValues(arr.map(object2PValue))
            } getOrElse PArray(Vector.empty[PValue])
          }
        },
        (rs, name) => {
          if (rs.getObject(name) == null) {
            PValue.Null
          } else {
            Try {
              val arr = getArray(rs.getArray(name).getArray.asInstanceOf[Any])
              PValue.fromValues(arr.map(object2PValue))
            } getOrElse PArray(Vector.empty[PValue])
          }
        }
      )
    } else ColumnReader.pure[PValue](
      (rs, i) => getPValue(rs.getObject(i), object2PValue),
      (rs, name) => getPValue(rs.getObject(name), object2PValue)
    )

  private def getPValue[A](a: A, factory: A => PValue): PValue = if (a == null) PValue.Null else factory(a)

  private def getArray(value: Any): Array[AnyRef] = {
    value match {
      case objArr: Array[AnyRef] => objArr
      case _ =>
        val arrlength = java.lang.reflect.Array.getLength(value)
        (0 until arrlength).map(i => java.lang.reflect.Array.get(value, i)).toArray[AnyRef]
    }
  }

  private def object2PValue(obj: Any): PValue = obj match {
    case x: java.lang.Boolean => PValue.fromBoolean(x)
    case x: java.lang.Integer => PValue.fromInt(x)
    case x: java.lang.Byte => PValue.fromInt(x.intValue())
    case x: java.lang.Short => PValue.fromInt(x.intValue())
    case x: java.lang.Long => PValue.fromLong(x)
    case x: java.lang.Number => PValue.fromDouble(x.doubleValue())
    case x: java.sql.Timestamp => PValue.fromInstant(x.toInstant)
    case x: java.sql.Date => PValue.fromLocalDate(x.toLocalDate)
    case null => PValue.Null
    case x => PValue.fromString(x.toString)
  }
}
