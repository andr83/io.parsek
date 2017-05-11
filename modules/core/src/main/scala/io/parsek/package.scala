package io

import io.parsek.PValue.PMap

/**
  * @author Andrei Tupitcyn
  */
package object parsek extends DefaultEncoders with DefaultDecoders {
  import PValue._
  @inline def arr(values: PValue*): PValue = PValue.arr(values:_*)
  @inline def pmap(fields: PValue.FieldType*): PMap = PValue.pmap(fields:_*)

  implicit val instantFormatter = InstantFormatter()
}
