package io.parsek.optics

import io.parsek.PValue._
import io.parsek._
import io.parsek.implicits._

import scala.language.dynamics

/**
  * Wrapper for [[Lens]] to add Dynamic support and lazy computation.
  *
  * @author Andrei Tupitcyn
  */
case class LensPath(private val lens: Lens[PValue, PValue])
  extends GetterPath(lens)
    with Dynamic {

  import io.parsek.optics.LensPath._

  /** Cast target PValue -> A. Using in [[Projection]] */
  override def as[A: Decoder : Encoder]: Lens[PValue, A] = Lens[PValue, A](value => {
    for (
      v <- lens.get(value);
      a <- implicitly[Decoder[A]].apply(v)
    ) yield a
  })(a => value => lens.set(implicitly[Encoder[A]].apply(a))(value))

  /** Extract value by key in source PMap */
  override def at(key: Symbol): LensPath = LensPath(lens compose (mapLens compose index(key)))

  /** Extract value by key in source PMap */
  override def at(key: String): LensPath = LensPath(lens compose (mapLens compose index(Symbol(key))))

  /** Replace target with new value in source PValue */
  def set(v: PValue): PValue => PResult[PValue] = lens.set(v)

  /** Replace target with new value in source PValue */
  def set[A: Encoder](a: A): PValue => PResult[PValue] = lens.set(implicitly[Encoder[A]].apply(a))

  override def selectDynamic(field: String): LensPath = LensPath(mapLens.compose(index(Symbol(field))))
}

object LensPath {
  val root = LensPath(Lens.id)

  def mapLens: Lens[PValue, Map[Symbol, PValue]] = lens[Map[Symbol, PValue]](PMap)

  def bytes: Lens[PValue, Array[Byte]] = lens[Array[Byte]](PBytes)

  def lens[A: Decoder](get: A => PValue): Lens[PValue, A] =
    Lens(implicitly[Decoder[A]].apply)(a => _ => PResult.catchNonFatal(get(a)))

  def index(key: Symbol): Lens[Map[Symbol, PValue], PValue] =
    Lens[Map[Symbol, PValue], PValue](
      v => v.get(key).fold[PResult[PValue]](PResult.invalid(TraverseFailure(s"Field $key doesn't exist")))(PResult.valid)
    )(
      a => v => PResult.valid(v + (key -> a))
    )
}


