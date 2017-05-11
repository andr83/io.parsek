package io.parsek.jackson

import com.fasterxml.jackson.databind.module.SimpleModule
import io.parsek.{InstantFormatter, PValue}

/**
  * @author andr83
  */
@SerialVersionUID(1L)
class ParsekModule(timeFormatter: InstantFormatter) extends SimpleModule {
  addSerializer(classOf[PValue], new PValueSerializer(timeFormatter))
  addDeserializer(classOf[PValue], new PValueDeserializer)
}
