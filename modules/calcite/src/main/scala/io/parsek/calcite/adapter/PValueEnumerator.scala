package io.parsek.calcite.adapter

import java.lang.{Iterable => JIterable}
import java.util

import org.apache.calcite.linq4j.{Enumerator, Linq4j}

/**
  * @author Andrei Tupitcyn
  */
class PValueEnumerator(source: util.List[Array[AnyRef]]) extends Enumerator[Array[AnyRef]] {

  private val enumerator = Linq4j.enumerator(source)

  override def close(): Unit = {
    try {
      enumerator.close()
    } catch {
      case e: Exception => throw new RuntimeException("Error closing enumerator", e);
    }
  }

  override def moveNext(): Boolean = enumerator.moveNext()

  override def current(): Array[AnyRef] = enumerator.current()

  override def reset(): Unit = enumerator.reset()
}