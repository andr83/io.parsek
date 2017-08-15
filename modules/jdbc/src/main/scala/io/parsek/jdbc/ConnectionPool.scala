package io.parsek.jdbc

import java.sql.Connection
import javax.sql.DataSource

import scala.language.implicitConversions

/**
  * @author Andrei Tupitcyn
  */
trait ConnectionPool {
  def getConnection: Connection

  def withConnection[A](f: Connection => A): A = {
    val c = getConnection
    try {
      f(c)
    } finally {
      c.close()
    }
  }
}

object ConnectionPool {
  def apply(dataSource: DataSource): ConnectionPool = new ConnectionPool {
    override def getConnection: Connection = dataSource.getConnection()
  }

  def apply(connection: Connection): ConnectionPool = new ConnectionPool() {
    override def getConnection: Connection = connection
  }

  implicit def dataSourceToPool(dataSource: DataSource): ConnectionPool = ConnectionPool(dataSource)
  implicit def connectionToPool(connection: Connection): ConnectionPool = ConnectionPool(connection)
}