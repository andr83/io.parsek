package io.parsek.jdbc

/**
  * @author Andrei Tupitcyn
  *
  * Represent how type `A` should be converted to sql fragment with placeholder.
  * Return sql fragment and count of placeholders
  */
trait ToSql[A] extends (A => (String, Int))
