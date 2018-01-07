package io.parsek.serde

import io.parsek.{PResult, PValue}

/** Base trait for serializer from `PValue` to String  */
trait Serializer {
  def write(value: PValue): String
}

/** Base trait to read `PResult` from String source */
trait Deserializer {
  def read(value: String): PResult[PValue]
}

/**
  * @author Andrei Tupitcyn
 */
abstract class SerDe extends Serializer with Deserializer
