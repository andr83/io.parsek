package io.parsek.jackson

import com.fasterxml.jackson.databind.module.SimpleModule
import io.parsek.{PValue, PValueFormatter}

/**
  * @author andr83
  */
@SerialVersionUID(1L)
class ParsekModule(formatter: PValueFormatter) extends SimpleModule {
  addSerializer(classOf[PValue], new PValueSerializer(formatter))
  addDeserializer(classOf[PValue], new PValueDeserializer)
}
