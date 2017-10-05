package io.parsek.shapeless

/**
  * Created by bfattahov on 04/10/2017.
  */
trait Configuration {
  def allowAdditionalFields: Boolean = true

  def useDefaults: Boolean = true

  def tryPNullForEmptyFields: Boolean = true

  def lookUpFieldName: Symbol => Symbol = identity
}

object Configuration {

  trait Weak {

    implicit val weak: Configuration = new Configuration {}
  }

  trait Strict {

    implicit val strict: Configuration = new Configuration {
      override def allowAdditionalFields: Boolean = false

      override def tryPNullForEmptyFields = false
    }
  }

  object Weak extends Weak

  object Strict extends Strict

}
