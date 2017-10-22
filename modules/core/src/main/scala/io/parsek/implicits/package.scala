package io.parsek

/**
  * @author Andrei Tupitcyn
  */
package object implicits extends syntax.PValueSyntax
  with syntax.TraversableSyntax
  with instances.DecoderInstances
  with instances.EncoderInstances
  with instances.PMapInstances
