package io.parsek.jackson

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import io.parsek.PValue._
import io.parsek.{PValue, PValueFormatter}

/**
  * @author Andrei Tupitcyn
  */
class PValueSerializer(formatter: PValueFormatter) extends JsonSerializer[PValue] {

  def this() = this(PValueFormatter())

  override def serialize(value: PValue, gen: JsonGenerator, serializers: SerializerProvider): Unit = value match {
    case PNull =>
      gen.writeNull()
    case PBoolean(v) =>
      gen.writeBoolean(v)
    case PInt(v) =>
      gen.writeNumber(formatter.formatInt(v))
    case PLong(v) =>
      gen.writeNumber(formatter.formatLong(v))
    case PDouble(v) =>
      gen.writeNumber(formatter.formatDouble(v))
    case PString(v) =>
      gen.writeString(v)
    case PInstant(v) =>
      formatter.formatInstant(v) match {
        case Left(l) => gen.writeNumber(l)
        case Right(str) => gen.writeString(str)
      }
    case PDate(v) => gen.writeString(formatter.formatDate(v))
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
