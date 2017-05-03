package io.parsek

import java.time.Instant

import io.parsek.PValue._
import io.parsek.optics._
import PPath._

import scala.util.Try
import scala.language.dynamics

/**
  * @author andr83
  */
case class PPath(value: Optional[PValue, PValue]) extends Dynamic {
  def `null`: Optional[PValue, Unit] = value compose pNull
  def boolean: Optional[PValue, Boolean] = value compose pBoolean
  def int: Optional[PValue, Int] = value compose pInt
  def long: Optional[PValue, Long] = value compose pLong
  def double: Optional[PValue, Double] = value compose pDouble
  def string: Optional[PValue, String] = value compose pString
  def time: Optional[PValue, Instant] = value compose pTime
  def arr: Optional[PValue, Vector[PValue]] = value compose pArray
  def pmap: Optional[PValue, Map[String, PValue]] = value compose pMap

  def at(key: String): PPath = PPath(pmap compose index(key))

  def selectDynamic(field: String): PPath = {
    val o = index(field)
    PPath(pmap.compose(o))
  }
}

object PPath {
  val root = PPath(Optional.id)

  val pNull = Prism.partial[PValue, Unit]{case Null=> ()}(_ => Null)

  val pBoolean = Prism.partialOption[PValue, Boolean]{
    case PBoolean(v) => Some(v)
    case PString(str) => Try(str.toBoolean).toOption
  }(PBoolean)

  val pInt = Prism.partialOption[PValue, Int] {
    case PInt(v) => Some(v)
    case PLong(v) => Some(v.toInt)
    case PDouble(v) => Some(v.toInt)
    case PString(v)=> Try(v.toInt).toOption
  }(PInt)

  val pLong = Prism.partialOption[PValue, Long] {
    case PInt(v) => Some(v.toLong)
    case PLong(v) => Some(v)
    case PDouble(v) => Some(v.toLong)
    case PString(v)=> Try(v.toLong).toOption
    case PTime(v) => Some(v.toEpochMilli)
  }(PLong)

  val pDouble = Prism.partialOption[PValue, Double] {
    case PInt(v) => Some(v.toDouble)
    case PLong(v) => Some(v.toDouble)
    case PDouble(v) => Some(v)
    case PString(v)=> Try(v.toDouble).toOption
  }(PDouble)

  val pString = Prism.partial[PValue, String] {
    case PString(v) => v
    case PBoolean(v) => v.toString
    case PInt(v) => v.toString
    case PLong(v) => v.toString
    case PDouble(v) => v.toString
    case PBoolean(v) => v.toString
    case PBoolean(v) => v.toString
  }(PString)

  val pTime = Prism.partial[PValue, Instant] {
    case PTime(v) => v
    case PLong(v) => Instant.ofEpochMilli(v)
  }(PTime)

  val pArray = Prism.partial[PValue, Vector[PValue]] {
    case PArray(v) => v
  }(PArray)

  val pMap = Prism.partial[PValue, Map[String, PValue]] {
    case PMap(v) => v
  }(PMap)

  def index(key: String): Optional[Map[String, PValue], PValue] =
    Optional[Map[String, PValue], PValue](v => v.get(key)) {a => v => v + (key -> a) }
}


