package io.parsek.shapeless

import io.parsek.syntax.encoder

/**
  * @author andr83
  */
package object implicits extends instances.HListDecoder
    with instances.HListEncoder
    with encoder
    with Configuration.Weak
