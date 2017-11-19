package io.parsek.optics

import java.time.Instant

import io.parsek.PValue._
import io.parsek._
import io.parsek.implicits._

import scala.language.dynamics

/**
  * @author andr83
  */
case class LensPath(private val lens: Lens[PValue, PValue])
  extends GetterPath(lens)
    with Dynamic {

  import io.parsek.optics.LensPath._

  override def as[A: Decoder : Encoder]: Lens[PValue, A] = Lens[PValue, A](value => {
    for (
      v <- lens.get(value);
      a <- implicitly[Decoder[A]].apply(v)
    ) yield a
  })(a => value => lens.set(implicitly[Encoder[A]].apply(a))(value))

  override def at(key: Symbol): LensPath = LensPath(mapLens compose index(key))

  override def at(key: String): LensPath = LensPath(mapLens compose index(Symbol(key)))

  def set(v: PValue): PValue => PResult[PValue] = lens.set(v)

  def set[A: Encoder](a: A): PValue => PResult[PValue] = lens.set(implicitly[Encoder[A]].apply(a))

  override def selectDynamic(field: String): LensPath = LensPath(mapLens.compose(index(Symbol(field))))
}

object LensPath {
  val root = LensPath(Lens.id)

  def nullLens: Lens[PValue, Unit] = lens[Unit](_ => PValue.Null)

  def booleanLens: Lens[PValue, Boolean] = lens[Boolean](PBoolean)

  def intLens: Lens[PValue, Int] = lens[Int](PInt)

  def longLens: Lens[PValue, Long] = lens[Long](PLong)

  def doubleLens: Lens[PValue, Double] = lens[Double](PDouble)

  def stringLens: Lens[PValue, String] = lens[String](PString)

  def instantLens: Lens[PValue, Instant] = lens[Instant](PInstant)

  def vectorLens: Lens[PValue, Vector[PValue]] = lens[Vector[PValue]](PArray)

  def lens[A: Decoder](get: A => PValue): Lens[PValue, A] =
    Lens(implicitly[Decoder[A]].apply)(a => _ => PResult.catchNonFatal(get(a)))

  def mapLens: Lens[PValue, Map[Symbol, PValue]] = lens[Map[Symbol, PValue]](PMap)

  def bytes: Lens[PValue, Array[Byte]] = lens[Array[Byte]](PBytes)

  def index(key: Symbol): Lens[Map[Symbol, PValue], PValue] =
    Lens[Map[Symbol, PValue], PValue](
      v => v.get(key).fold[PResult[PValue]](PResult.invalid(TraverseFailure(s"Field $key doesn't exist")))(PResult.valid)
    )(
      a => v => PResult.valid(v + (key -> a))
    )
}


