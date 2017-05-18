package io

import io.parsek.PValue.PMap
import io.parsek.instances.{DecoderInstances, EncoderInstances}
import io.parsek.optics.PPath

/**
  * @author Andrei Tupitcyn
  */
package object parsek extends EncoderInstances with DecoderInstances {
  @inline def arr(values: PValue*): PValue = PValue.arr(values:_*)
  @inline def pmap(fields: PValue.FieldType*): PMap = PValue.pmap(fields:_*)
  @inline val root: PPath = PPath.root
}
