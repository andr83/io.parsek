package io.parsek.jdbc.generic.instances

import io.parsek.jdbc.{ParameterTypeMeta, ToSql, ValueBinder}

/**
  * @author andr83
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
