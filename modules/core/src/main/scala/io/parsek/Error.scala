package io.parsek

/**
  * @author Andrei Tupitcyn
  */
sealed abstract class Error extends Exception {
  final override def fillInStackTrace(): Throwable = this
}

final case class TypeCastFailure(message: String) extends Error

final case class ParsingFailure(message: String, underlying: Throwable) extends Error {}

final case class TraverseFailure(message: String) extends Error {}
