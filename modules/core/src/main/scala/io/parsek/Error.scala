package io.parsek

/**
  * @author Andrei Tupitcyn
  */
sealed abstract class Error(message: String, underlying: Throwable) extends Exception(message, underlying) {
  final override def fillInStackTrace(): Throwable = this
}

final case class TypeCastFailure(message: String) extends Error(message, null)

final case class ParsingFailure(message: String, underlying: Throwable) extends Error(message, underlying)
final case class TraverseFailure(message: String) extends Error(message, null)

final case class NullValue(message: String) extends Error(message, null)

final case class NullField(field: Symbol, message: String) extends Error(message, null)

final case object FilterFailure extends Error("FilterFailure", null)
