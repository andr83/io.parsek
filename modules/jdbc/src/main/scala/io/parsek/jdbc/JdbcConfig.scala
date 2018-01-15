package io.parsek.jdbc

/**
  * @author Andrei Tupitcyn
  */
case class JdbcConfig(
  nameConverter: NameConverter = IdNameConverter
)
