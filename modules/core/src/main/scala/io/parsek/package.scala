package io

import io.parsek.PValue.PMap
import io.parsek.optics.PPath

/**
  * @author Andrei Tupitcyn
  */
package object parsek {
  @inline def arr(values: PValue*): PValue = PValue.arr(values:_*)
  @inline def pmap(fields: PValue.FieldType*): PMap = PValue.pmap(fields:_*)
  @inline val root: PPath = PPath.root
}
