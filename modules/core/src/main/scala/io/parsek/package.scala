package io

/**
  * @author Andrei Tupitcyn
  */
package object parsek extends DefaultEncoders with DefaultDecoders {
  @inline def arr(values: PValue*): PValue = PValue.arr(values:_*)
  @inline def pmap(fields: PValue.FieldType*): PValue = PValue.pmap(fields:_*)
}
