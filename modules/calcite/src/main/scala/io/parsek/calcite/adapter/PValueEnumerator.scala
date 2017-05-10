package io.parsek.calcite.adapter

import java.util.{List => JList}

import io.parsek.PValue.PMap
import org.apache.calcite.linq4j.{Enumerator, Linq4j}

/**
  * @author Andrei Tupitcyn
  */
class PValueEnumerator(source: JList[PMap]) extends Enumerator[Array[AnyRef]] {

  val enumerator = Linq4j.enumerator(source)

  override def close(): Unit = {
    try {
      enumerator.close()
    } catch {
      case e: Exception => throw new RuntimeException("Error closing enumerator", e);
    }
  }

  override def moveNext(): Boolean = enumerator.moveNext()

  override def current(): Array[AnyRef] = enumerator.current().value.values.map(_.toString).toArray

  override def reset(): Unit = enumerator.reset()
}
