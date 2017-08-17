package io.parsek.jackson

import com.fasterxml.jackson.databind.ObjectMapper
import cats.syntax.either._
import io.parsek.{PValue, PValueFormatter}
import io.parsek.serde.SerDe

/**
  * @author andr83 
  *         created on 15.05.17
  */
case class JsonSerDe(mapper: ObjectMapper) extends SerDe{

  override def read(value: String): PValue = mapper.readValue(value, classOf[PValue])

  override def write(value: PValue): String = mapper.writeValueAsString(value)

  def parseJson(json: String): Throwable Either PValue = Either.catchNonFatal(read(json))
}

object JsonSerDe {
  def apply(): JsonSerDe = JsonSerDe(PValueFormatter())
  def apply(formatter: PValueFormatter): JsonSerDe =
    JsonSerDe(new ObjectMapper().registerModule(new ParsekModule(formatter)))
}
