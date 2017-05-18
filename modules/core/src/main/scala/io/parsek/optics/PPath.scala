package io.parsek.optics

import java.time.Instant

import cats.syntax.either._
import io.parsek.PValue._
import io.parsek.instances.DecoderInstances
import io.parsek.optics.PPath.PValidated
import io.parsek.{Decoder, Encoder, FilterFailure, PValue, TraverseFailure}

import scala.language.dynamics

/**
  * @author andr83
  */
case class PPath(value: PValidated[PValue]) extends Dynamic {
  import io.parsek.optics.PPath._

  def `null`: PValidated[Unit] = value compose pNull

  def boolean: PValidated[Boolean] = value compose pBoolean

  def int: PValidated[Int] = value compose pInt

  def long: PValidated[Long] = value compose pLong

  def double: PValidated[Double] = value compose pDouble

  def string: PValidated[String] = value compose pString

  def time: PValidated[Instant] = value compose pTime

  def arr: PValidated[Vector[PValue]] = value compose pArray

  def pmap: PValidated[Map[Symbol, PValue]] = value compose pMap

  def bytes: PValidated[Array[Byte]] = value compose pBytes

  def at(key: Symbol): PPath = PPath(pmap compose index(key))

  def at(key: String): PPath = PPath(pmap compose index(Symbol(key)))

  def map[A: Decoder, B: Encoder](f: A => B): PPath = PPath(PValidation[PValue, PValue, Throwable, PValue, PValue](
    s => value._getOrModify(s)
      .flatMap(pa =>
        implicitly[Decoder[A]].apply(pa).map(
          a => implicitly[Encoder[B]].apply(f(a))).left.map[(Throwable, PValue)](e => e -> s)
      )
  )(value._set))

  def mapT[A: Decoder, B: Encoder](f: A => Throwable Either B): PPath = PPath(PValidation[PValue, PValue, Throwable, PValue, PValue](
    s => value._getOrModify(s)
      .flatMap(pa => {
        implicitly[Decoder[A]].apply(pa).flatMap(a => {
          f(a).map(implicitly[Encoder[B]].apply)
        }).left.map[(Throwable, PValue)](e => e -> s)
      })
  )(value._set))

  def filter[A : Decoder](f: A => Boolean): PPath = PPath(PValidation[PValue, PValue, Throwable, PValue, PValue](
    s => value._getOrModify(s)
      .flatMap(pa => {
        implicitly[Decoder[A]].apply(pa).flatMap(a => {
          if (f(a)) {
            Right(pa)
          } else {
            Left(FilterFailure)
          }
        }).left.map[(Throwable, PValue)](e => e -> s)
      })
  )(value._set))

  /**
    * Memoize is a lens with caching of getting result.
    * Can be usefull for getting access optimisation to source value inner fields
    *
    * @return
    */
  def memoize: PPath = {
    // Source argument which result need to memoize
    var source: Option[PValue] = None
    var result: Option[(Throwable, PValue) Either PValue] = None
    PPath(PValidation[PValue, PValue, Throwable, PValue, PValue](s => {
      (for {
        _source <- source
        if _source == s
        _result <- result
      } yield _result) getOrElse {
        val _result = value._getOrModify(s)
        source = Some(s)
        result = Some(_result)
        _result
      }
    })(value._set))
  }

  def set(v: PValue): PValue => PValue = value.set(v)

  def orElse(fallback: PPath): PPath = PPath(Validation.apply[PValue, Throwable, PValue]
    (s => value.get(s).orElse(fallback.value.get(s)))(value._set))

  def selectDynamic(field: String): PPath = PPath(pmap.compose(index(Symbol(field))))
}

object PPath extends DecoderInstances {
  type PValidated[A] = Validation[PValue, Throwable, A]
  val root = PPath(Validation.id)

  def validated[A: Decoder](reverseGet: A => PValue): PValidated[A] =
    Validation(implicitly[Decoder[A]].apply)(a => _ => reverseGet(a))

  def pNull: PValidated[Unit] = validated[Unit](_ => Null)

  def pBoolean: PValidated[Boolean] = validated[Boolean](PBoolean)

  def pInt: PValidated[Int] = validated[Int](PInt)

  def pLong: PValidated[Long] = validated[Long](PLong)

  def pDouble: PValidated[Double] = validated[Double](PDouble)

  def pString: PValidated[String] = validated[String](PString)

  def pTime: PValidated[Instant] = validated[Instant](PTime)

  def pArray: PValidated[Vector[PValue]] = validated[Vector[PValue]](PArray)

  def pMap: PValidated[Map[Symbol, PValue]] = validated[Map[Symbol, PValue]](PMap)

  def pBytes: PValidated[Array[Byte]] = validated[Array[Byte]](PBytes)

  def index(key: Symbol): Validation[Map[Symbol, PValue], Throwable, PValue] =
    Validation[Map[Symbol, PValue], Throwable, PValue](v =>
      v.get(key).fold[Throwable Either PValue](Left(TraverseFailure(s"Field $key doesn't exist")))(a => Right(a))
    ) { a => v => v + (key -> a) }
}


