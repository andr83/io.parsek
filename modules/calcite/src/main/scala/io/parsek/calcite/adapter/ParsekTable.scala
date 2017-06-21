package io.parsek.calcite.adapter

import java.util

import io.parsek.PValue._
import io.parsek.types._
import org.apache.calcite.DataContext
import org.apache.calcite.linq4j.{AbstractEnumerable, Enumerable, Enumerator, Linq4j}
import org.apache.calcite.rel.`type`.{RelDataType, RelDataTypeFactory}
import org.apache.calcite.schema.ScannableTable
import org.apache.calcite.schema.impl.AbstractTable

import collection.JavaConverters._

/**
  * @author Andrei Tupitcyn
  */
case class ParsekTable(source: util.Map[util.List[String], Array[AnyRef]], schema: PStructType, pk: Array[String]) extends AbstractTable with ScannableTable {

  import ParsekTable._

  val names: Seq[String] = schema.fields.map(_.name.name)
  val keys: Seq[Int] = pk.map(k=> schema.fields.indexWhere(_.name.name == k))
  val decoders: Seq[JavaTypeDecoder] = schema.fields.map(f => javaTypeDecoder(f.name, f.dataType))

  override def scan(root: DataContext): Enumerable[Array[AnyRef]] = new AbstractEnumerable[Array[AnyRef]] {
    override def enumerator(): Enumerator[Array[AnyRef]] = Linq4j.enumerator(source.values())
  }

  override def getRowType(typeFactory: RelDataTypeFactory): RelDataType = {
    val b = typeFactory.builder()
    schema.fields.foreach(f => {
      b.add(f.name.name, createParsekType(typeFactory, f.dataType))
    })
    b.build()
  }

  def key(value: Array[AnyRef]): util.List[String] = util.Collections.unmodifiableList(keys.map(i=> value(i).toString).asJava)
  def value(r: PMap): Array[AnyRef] = decoders.map(_.apply(r)).toArray

  def add(r: PMap): Unit = {
    val v = value(r)
    source.put(key(v), v)
  }

  def remove(r: PMap): Unit = {
    source.remove(key(value(r)))
  }
}

object ParsekTable {

  import io.parsek.instances.DecoderInstances._

  type JavaTypeDecoder = PMap => AnyRef

  def apply(schema: PStructType, keys: Seq[String]): ParsekTable =
    new ParsekTable(new util.HashMap[util.List[String], Array[AnyRef]](), schema, keys.toArray)

  def createParsekType(typeFactory: RelDataTypeFactory, dataType: PType): RelDataType = dataType match {
    case PBooleanType => typeFactory.createJavaType(classOf[Boolean])
    case PIntType => typeFactory.createJavaType(classOf[Int])
    case PLongType => typeFactory.createJavaType(classOf[Long])
    case PDoubleType => typeFactory.createJavaType(classOf[Double])
    case PInstantType => typeFactory.createJavaType(classOf[java.sql.Timestamp])
    case PStringType => typeFactory.createJavaType(classOf[String])
    case _ => throw new IllegalArgumentException(s"Type $dataType doesn't support")
  }

  def javaTypeDecoder(name: Symbol, dataType: PType): JavaTypeDecoder = {
    val decoder = dataType match {
      case PBooleanType => booleanDecoder
      case PIntType => intDecoder
      case PLongType => longDecoder
      case PDoubleType => doubleDecoder
      case PStringType => stringDecoder
      case PInstantType => timestampDecoder
      case PBinaryType => bytesDecoder
      case PArrayType => vectorDecoder
      case PMapType => mapDecoder
    }
    (pm: PMap) => pm.value.get(name).map(v => decoder(v).right.get.asInstanceOf[AnyRef]).orNull
  }
}
