package io.parsek.jackson

import com.fasterxml.jackson.databind.ObjectMapper
import io.parsek.serde.SerDe
import io.parsek.{PResult, PValue, PValueFormatter}

/**
  * @author Andrei Tupitcyn
  *         created on 15.05.17
  */
case class JsonSerDe(mapper: ObjectMapper) extends SerDe {

  override def write(value: PValue): String = mapper.writeValueAsString(value)

  @inline def parseJson(json: String): PResult[PValue] = read(json)

  override def read(value: String): PResult[PValue] = PResult.catchNonFatal(mapper.readValue(value, classOf[PValue]))
}

object JsonSerDe {
  def apply(): JsonSerDe = JsonSerDe(PValueFormatter())

  def apply(formatter: PValueFormatter): JsonSerDe =
    JsonSerDe(new ObjectMapper().registerModule(new ParsekModule(formatter)))
}
