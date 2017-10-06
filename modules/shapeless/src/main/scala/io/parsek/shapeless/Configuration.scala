package io.parsek.shapeless

/**
  * Created by bfattahov on 04/10/2017.
  */
trait Configuration {
  // Allow in PMap fields which are not defined in source/target class
  def allowAdditionalFields: Boolean = true
  // Try to set defaults in target class
  def useDefaults: Boolean = true

  def tryPNullForEmptyFields: Boolean = true

  def lookUpFieldName: Symbol => Symbol = identity
}

object Configuration {

  // Weak configuration doesn't impose restrictions on decoding/encoding process
  // and always try to use defaults if data doesn't exist.
  // Using by default
  trait Weak {

    implicit val parsekShapelessConf: Configuration = new Configuration {}
  }

  trait Strict extends Weak {

    override implicit val parsekShapelessConf: Configuration = new Configuration {
      override def allowAdditionalFields: Boolean = false

      override def tryPNullForEmptyFields = false
    }
  }

  object Weak extends Weak

  object Strict extends Strict
}
