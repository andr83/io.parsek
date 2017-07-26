package io.parsek.jackson

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import io.parsek.PValue._
import io.parsek.{InstantFormatter, PValue}

/**
  * @author Andrei Tupitcyn
  */
class PValueSerializer(timeFormatter: InstantFormatter) extends JsonSerializer[PValue] {

  def this() = this(InstantFormatter())

  override def serialize(value: PValue, gen: JsonGenerator, serializers: SerializerProvider): Unit = value match {
    case PNull =>
      gen.writeNull()
    case PBoolean(v) =>
      gen.writeBoolean(v)
    case PInt(v) =>
      gen.writeNumber(v)
    case PLong(v) =>
      gen.writeNumber(v)
    case PDouble(v) =>
      gen.writeNumber(v)
    case PString(v) =>
      gen.writeString(v)
    case PInstant(v) =>
      timeFormatter.format(v) match {
        case Left(l) => gen.writeNumber(l)
        case Right(str) => gen.writeString(str)
      }
    case PBytes(v) =>
      gen.writeBinary(v)
    case PArray(v) =>
      gen.writeStartArray(v.size)
      val it = v.iterator
      while (it.hasNext) {
        serialize(it.next(), gen, serializers)
      }
      gen.writeEndArray()
    case PMap(m) =>
      gen.writeStartObject()
      val it = m.iterator
      while (it.hasNext) {
        val (k, v) = it.next()
        gen.writeFieldName(k.name)
        serialize(v, gen, serializers)
      }
      gen.writeEndObject()
  }
}
