package io.parsek

/**
  * @author Andrei Tupitcyn
  */
package object implicits extends syntax.pvalue
  with syntax.traversable
  with instances.decoders
  with instances.encoders
  with instances.pmap
