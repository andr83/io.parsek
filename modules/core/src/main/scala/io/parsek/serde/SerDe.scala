package io.parsek.serde

import io.parsek.PValue


trait Serializer {
  def write(value: PValue): String
}

trait Deserializer {
  def read(value: String): PValue
}

/**
 * @author andr83
 */
abstract class SerDe extends Serializer with Deserializer
