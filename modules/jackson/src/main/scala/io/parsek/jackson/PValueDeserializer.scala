package io.parsek.jackson

import com.fasterxml.jackson.core.{JsonParser, TreeNode}
import com.fasterxml.jackson.databind.node._
import com.fasterxml.jackson.databind.{DeserializationContext, JsonDeserializer, JsonNode}
import io.parsek.{InstantFormatter, PValue}

/**
  * @author andr83 
  *         created on 15.05.17
  */
class PValueDeserializer extends JsonDeserializer[PValue]{
  override def deserialize(p: JsonParser, ctxt: DeserializationContext): PValue = {
    convert(p.getCodec.readTree[TreeNode](p))
  }

  def convert(node: TreeNode): PValue = node match {
    case _: NullNode => PValue.Null
    case x: ShortNode => PValue.fromInt(x.intValue())
    case x: IntNode => PValue.fromInt(x.intValue())
    case x: LongNode => PValue.fromLong(x.longValue())
    case x: BigIntegerNode => PValue.fromLong(x.longValue())
    case x: FloatNode => PValue.fromDouble(x.doubleValue())
    case x: DoubleNode => PValue.fromDouble(x.doubleValue())
    case x: DecimalNode => PValue.fromDouble(x.doubleValue())
    case x: BooleanNode => PValue.fromBoolean(x.booleanValue())
    case x: TextNode => PValue.fromString(x.textValue())
    case x: BinaryNode => PValue.fromBytes(x.binaryValue())
    case x: ArrayNode =>
      val it = x.elements()
      var arr = Vector.empty[PValue]
      while (it.hasNext) {
        arr = arr :+ convert(it.next())
      }
      PValue.fromValues(arr)
    case x: ObjectNode =>
      val it = x.fields()
      var map = Map.empty[Symbol, PValue]
      while (it.hasNext) {
        val entry = it.next()
        map = map + (Symbol(entry.getKey) -> convert(entry.getValue))
      }
      PValue.fromMap(map)
  }
}
