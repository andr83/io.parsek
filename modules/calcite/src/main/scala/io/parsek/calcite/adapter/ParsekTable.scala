package io.parsek.calcite.adapter

import java.util

import io.parsek.PValue.PMap
import org.apache.calcite.DataContext
import org.apache.calcite.linq4j.{AbstractEnumerable, Enumerable, Enumerator}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.ScannableTable
import org.apache.calcite.schema.impl.AbstractTable

/**
  * @author Andrei Tupitcyn
  */
class ParsekTable(source: util.List[PMap]) extends AbstractTable with ScannableTable {

  override def scan(root: DataContext): Enumerable[Array[AnyRef]] = new AbstractEnumerable[Array[AnyRef]]() {
    def enumerator(): Enumerator[Array[AnyRef]] = {
      new PValueEnumerator(source)
    }
  }

  override def getRowType(typeFactory: RelDataTypeFactory): RelDataType =
    typeFactory.builder().add("name", typeFactory.createJavaType(classOf[String])).build()
}
