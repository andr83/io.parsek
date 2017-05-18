package io.parsek.jdbc.free

import cats.free.Free
import cats.free.Free.liftF

/**
  * @author andr83 
  *         created on 15.05.17
  */
object dsl {
  sealed trait JdbcOpA[A]
  case class Query[A](sql: String, params: Seq[Any] = Seq.empty[Any]) extends JdbcOpA[Option[A]]
  case class Insert(sql: String, params: Seq[Any] = Seq.empty[Any]) extends JdbcOpA[Unit]

  type JdbcOp[A] = Free[JdbcOpA, A]

  def query[A](sql: String, params: Seq[Any] = Seq.empty[Any]): JdbcOp[Option[A]] =
    liftF[JdbcOpA, Option[A]](Query[A](sql, params))
}
