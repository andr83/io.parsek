package io

import cats.data.ValidatedNel
import io.parsek.PValue.PMap
import io.parsek.optics.PPath
import io.parsek.types.PStructType

/**
  * @author Andrei Tupitcyn
  */
package object parsek {
  @inline val root: PPath = PPath.root

  @inline def arr(values: PValue*): PValue = PValue.arr(values:_*)

  @inline def pmap(fields: PValue.FieldType*): PMap = PValue.pmap(fields:_*)

  def validate(root: PMap, schema: PStructType): ValidatedNel[Throwable, PValue] = {
    var res = Map.empty[Symbol, PValue]
    schema.fields.map()
  }
}
