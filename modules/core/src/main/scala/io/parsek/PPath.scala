package io.parsek

import java.time.Instant

import io.parsek.PValue._
import io.parsek.optics._
import PPath._

import scala.language.dynamics

/**
  * @author andr83
  */
case class PPath(value: PValidated[PValue]) extends Dynamic {

  def `null`: PValidated[Unit] = value compose pNull
  def boolean: PValidated[Boolean] = value compose pBoolean
  def int: PValidated[Int] = value compose pInt
  def long: PValidated[Long] = value compose pLong
  def double: PValidated[Double] = value compose pDouble
  def string: PValidated[String] = value compose pString
  def time: PValidated[Instant] = value compose pTime
  def arr: PValidated[Vector[PValue]] = value compose pArray
  def pmap: PValidated[Map[String, PValue]] = value compose pMap
  def bytes: PValidated[Array[Byte]] = value compose pBytes

  def at(key: String): PPath = PPath(pmap compose index(key))

  def selectDynamic(field: String): PPath = PPath(pmap.compose(index(field)))
}

object PPath extends DefaultDecoders {
  type PValidated[A] = Validation[PValue, Throwable, A]
  val root = PPath(Validation.id)

  def validated[A : Decoder](reverseGet: A => PValue): PValidated[A] =
    Validation(implicitly[Decoder[A]].apply)(a => _ => reverseGet(a))

  def pNull = validated[Unit](_ => Null)
  def pBoolean = validated[Boolean](PBoolean)
  def pInt = validated[Int](PInt)
  def pLong = validated[Long](PLong)
  def pDouble = validated[Double](PDouble)
  def pString = validated[String](PString)
  def pTime = validated[Instant](PTime)
  def pArray = validated[Vector[PValue]](PArray)
  def pMap = validated[Map[String, PValue]](PMap)
  def pBytes = validated[Array[Byte]](PBytes)

  def index(key: String): Validation[Map[String, PValue], Throwable, PValue] =
    Validation[Map[String, PValue], Throwable, PValue](v =>
      v.get(key).fold[Throwable Either PValue](Left(TraverseFailure(s"Field $key doesn't exist")))(a=>Right(a))
    ) {a => v => v + (key -> a) }
}


