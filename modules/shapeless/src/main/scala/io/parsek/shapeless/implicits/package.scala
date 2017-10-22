package io.parsek.shapeless

import io.parsek.syntax.EncoderSyntax

/**
  * @author andr83
  */
package object implicits extends instances.HListDecoder
    with instances.HListEncoder
    with EncoderSyntax
    with Configuration.Weak
