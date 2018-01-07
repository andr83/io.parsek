package io.parsek.jdbc.instances

import io.parsek.jdbc.ToSql

/**
  * @author Andrei Tupitcyn
  */

trait ToSqlInstances0 {
  implicit def defaultToSql[A] = new ToSql[A] {
    override def apply(a: A): (String, Int) = ("?", 1)
  }
}

trait ToSqlInstances extends ToSqlInstances0 {
  implicit def traversableToSql[A, T <: Traversable[A]]: ToSql[T] = new ToSql[T] {
    override def apply(v: T): (String, Int) = v.map(_ => "?").mkString(",") -> v.size
  }

  implicit def seqToSql[A]: ToSql[Seq[A]] = traversableToSql[A, Seq[A]]
}
