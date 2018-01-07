package io.parsek.shapeless

/**
  * @author Andrei Tupitcyn
  */
package object implicits extends instances.HListDecoder
    with instances.HListEncoder
    with Configuration.Weak
