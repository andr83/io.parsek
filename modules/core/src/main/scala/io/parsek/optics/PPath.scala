package io.parsek.optics

import java.time.Instant

import io.parsek.PValue._
import io.parsek._
import io.parsek.implicits._

import scala.language.dynamics

/**
  * @author andr83
  */
case class PPath(private val lens: Lens[PValue, PValue]) extends Dynamic {

  import io.parsek.optics.PPath._

  def asOpt[A: Decoder : Encoder]: Lens[PValue, Option[A]] = Lens[PValue, Option[A]](value => {
    implicitly[Decoder[Option[A]]].apply(lens.get(value).getOrElse(value))
  })(a => value => lens.set(implicitly[Encoder[Option[A]]].apply(a))(value))

  def asOpt[A: Decoder](value: PValue): PResult[Option[A]] =
    (for(
      v  <- lens.get(value);
      oa <- implicitly[Decoder[Option[A]]].apply(v)
    ) yield oa)
      .fold(nel=> PResult.valid(None).withWarnings(nel.toList), res=> PResult.valid(res))

  def as[A: Decoder : Encoder]: Lens[PValue, A] = Lens[PValue, A](value => {
    for (
      v <- lens.get(value);
      a <- implicitly[Decoder[A]].apply(v)
    ) yield a
  })(a => value => lens.set(implicitly[Encoder[A]].apply(a))(value))

  def to[A: Decoder](value: PValue): A = {
    as[A](value)
      .fold(
        nel => throw nel.head,
        v => v
      )
  }

  def as[A: Decoder](value: PValue): PResult[A] = {
    for (
      value <- lens.get(value);
      a <- implicitly[Decoder[A]].apply(value)
    ) yield a
  }

  def at(key: Symbol): PPath = PPath(mapLens compose index(key))

  def at(key: String): PPath = PPath(mapLens compose index(Symbol(key)))

  def map[A: Decoder, B: Encoder](f: A => B): PPath = {
    val decoderA = implicitly[Decoder[A]]
    val encoderB = implicitly[Encoder[B]]
    PPath(Lens[PValue, PValue](
      s =>
        for {
          pa <- lens.get(s)
          a <- decoderA(pa)
        } yield encoderB(f(a))
    )(lens.set))
  }

  def transform[A: Decoder, B: Encoder](f: A => PResult[B]): PPath = {
    val decoderA = implicitly[Decoder[A]]
    val encoderB = implicitly[Encoder[B]]
    PPath(Lens[PValue, PValue](
      s =>
        for {
          pa <- lens.get(s)
          a <- decoderA(pa)
          b <- f(a)
        } yield encoderB(b)
    )(lens.set))
  }

  def filter[A: Decoder](f: A => Boolean): PPath = {
    val decoderA = implicitly[Decoder[A]]
    PPath(Lens[PValue, PValue](
      s =>
        for {
          pa <- lens.get(s)
          a <- decoderA(pa)
        } yield if (f(a)) pa else PValue.Null
    )(lens.set))
  }


  //  def findAndMap[A : Decoder, B: Encoder](p: (Symbol,PValue) => Boolean, f: (Symbol,A) => (Symbol, B)): PPath = {
  //    PPath(ValidationS[PValue, PValue, Throwable, PValue, PValue](
  //      s => value._getOrModify(s).map(_.findAndMap(p, f))
  //    )(value._set))
  //  }

  def set(v: PValue): PValue => PResult[PValue] = lens.set(v)

  def orElse(fallback: PPath): PPath = PPath(Lens[PValue, PValue]
    (s => lens.get(s).orElse(fallback.lens.get(s)))(lens.set))

  def selectDynamic(field: String): PPath = PPath(mapLens.compose(index(Symbol(field))))
}

object PPath {
  val root = PPath(Lens.id)

  def nullLens: Lens[PValue, Unit] = lens[Unit](_ => PValue.Null)

  def booleanLens: Lens[PValue, Boolean] = lens[Boolean](PBoolean)

  def intLens: Lens[PValue, Int] = lens[Int](PInt)

  def longLens: Lens[PValue, Long] = lens[Long](PLong)

  def doubleLens: Lens[PValue, Double] = lens[Double](PDouble)

  def lens[A: Decoder](get: A => PValue): Lens[PValue, A] =
    Lens(implicitly[Decoder[A]].apply)(a => _ => PResult.catchNonFatal(get(a)))

  def stringLens: Lens[PValue, String] = lens[String](PString)

  def instantLens: Lens[PValue, Instant] = lens[Instant](PInstant)

  def vectorLens: Lens[PValue, Vector[PValue]] = lens[Vector[PValue]](PArray)

  def mapLens: Lens[PValue, Map[Symbol, PValue]] = lens[Map[Symbol, PValue]](PMap)

  def bytes: Lens[PValue, Array[Byte]] = lens[Array[Byte]](PBytes)

  def index(key: Symbol): Lens[Map[Symbol, PValue], PValue] =
    Lens[Map[Symbol, PValue], PValue](
      v => v.get(key).fold[PResult[PValue]](PResult.invalid(TraverseFailure(s"Field $key doesn't exist")))(PResult.valid)
    )(
      a => v => PResult.valid(v + (key -> a))
    )
}


