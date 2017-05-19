package io.parsek.jdbc

import java.sql.Connection
import javax.sql.DataSource

/**
  * @author Andrei Tupitcyn
  */
case class ConnectionPool(dataSource: DataSource) {

  def getConnection: Connection = dataSource.getConnection

  def withConnection(f: Connection => Unit): Unit = {
    val c = getConnection
    try {
      f(c)
    } finally {
      c.close()
    }
  }
}
