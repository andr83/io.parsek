package io.parsek.optics

import io.parsek.PValue.PMap
import io.parsek._
import io.parsek.implicits._

import scala.language.dynamics

/**
  * [[GetterPath]] wrap [[Getter]][PValue, PValue] to add Dynamic and lazy computation.
  * It can be seen as shape for future evaluated [[Getter]]
  *
  * @author Andrei Tupitcyn
  */
class GetterPath(private val getter: Getter[PValue, PValue]) extends Dynamic {

  import LensPath._

  /** Return value from source */
  def get(s: PValue): PResult[PValue] = getter.get(s)

  /** Cast target PValue -> Option[A]. Using in [[Projection]] */
  def asOpt[A: Decoder : Encoder]: Getter[PValue, Option[A]] = Getter[PValue, Option[A]](value => {
    implicitly[Decoder[Option[A]]].apply(getter.get(value).getOrElse(PValue.Null))
  })

  /** Try safe cast target PValue -> Option[A] from source PValue */
  def asOpt[A: Decoder](value: PValue): PResult[Option[A]] =
    (for (
      v <- getter.get(value);
      oa <- implicitly[Decoder[Option[A]]].apply(v)
    ) yield oa)
      .fold(nel => PResult.valid(None).withWarnings(nel.toList), res => PResult.valid(res))

  /** Cast target PValue -> A. Using in [[Projection]] */
  def as[A: Decoder : Encoder]: Getter[PValue, A] = Getter[PValue, A](value => {
    for (
      v <- getter.get(value);
      a <- implicitly[Decoder[A]].apply(v)
    ) yield a
  })

  /** Try unsafe cast target PValue -> A from source PValue */
  def to[A: Decoder](value: PValue): A = {
    as[A](value)
      .fold(
        nel => throw nel.head,
        v => v
      )
  }

  /** Try safe cast target PValue -> A from source PValue */
  def as[A: Decoder](value: PValue): PResult[A] = {
    for (
      value <- getter.get(value);
      a <- implicitly[Decoder[A]].apply(value)
    ) yield a
  }

  /** Extract value by key in source PMap */
  def at(key: Symbol): GetterPath = LensPath(mapLens compose index(key))

  /** Extract value by key in source PMap */
  def at(key: String): GetterPath = LensPath(mapLens compose index(Symbol(key)))

  /**
    * Apply `f: A => B` to target PValue
    *
    * {{{
    * scala> val v = pmap('foo -> PValue(1))
    * v: io.parsek.PValue.PMap = PMap(Map('foo -> PInt(1)))
    *
    * scala> root.foo.map[Int, Int](x=> x * 100).get(v)
    * res1: io.parsek.PResult[io.parsek.PValue] = PSuccess(PInt(100),List())
    * }}}
    */
  def map[A: Decoder, B: Encoder](f: A => B): GetterPath = {
    val decoderA = implicitly[Decoder[A]]
    val encoderB = implicitly[Encoder[B]]
    GetterPath(s =>
      for {
        pa <- getter.get(s)
        a <- decoderA(pa)
      } yield encoderB(f(a))
    )
  }

  /** Try to transform target value `A => PResult[B]` */
  def transform[A: Decoder, B: Encoder](f: A => PResult[B]): GetterPath = {
    val decoderA = implicitly[Decoder[A]]
    val encoderB = implicitly[Encoder[B]]
    GetterPath(s =>
      for {
        pa <- getter.get(s)
        a <- decoderA(pa)
        b <- f(a)
      } yield encoderB(b)
    )
  }

  /** Filter target value by predicate. If predicate false instead of target value will return PValue.Null */
  def filter[A: Decoder](f: A => Boolean): GetterPath = {
    val decoderA = implicitly[Decoder[A]]
    GetterPath(s =>
      for {
        pa <- getter.get(s)
        a <- decoderA(pa)
      } yield if (f(a)) pa else PValue.Null
    )
  }

  /**
    * Find all entries in PMap recursively and return Traversal
    *
    * {{{
    * scala> val v = pmap('foo -> pmap('bar -> PValue(100), 'baz -> PValue(20), 'quux -> PValue("hello")), 'bat -> PValue(42))
    * v: io.parsek.PValue.PMap = PMap(Map('foo -> PMap(Map('bar -> PInt(100), 'baz -> PInt(20), 'quux -> PString(hello))), 'bat -> PInt(42)))
    *
    * scala> root.find[Int]{case (k,v)=> v > 50}.modify{case (k,v)=> k -> (v + 100) }(v)
    * res4: io.parsek.PResult[io.parsek.PValue] = PSuccess(PMap(Map('foo -> PMap(Map('bar -> PInt(200), 'baz -> PInt(20), 'quux -> PString(hello))), 'bat -> PInt(42))),List())
    * }}}
    */
  def find[A: Decoder : Encoder](p: ((Symbol, A)) => Boolean): TraversalPath[(Symbol, A)] = {
    val decoder = implicitly[Decoder[A]]
    val encoder = implicitly[Encoder[A]]

    def getAll(map: Map[Symbol, PValue]): Traversable[(Symbol, A)] = {
      map.toSeq.flatMap {
        case (_, PMap(innerMap)) => getAll(innerMap)
        case (k, v) => decoder(v).map(a => if (p(k -> a)) Seq(k -> a) else Seq.empty[(Symbol, A)]).getOrElse(Seq.empty[(Symbol, A)])
      }
    }

    def modify(map: Map[Symbol, PValue], f: ((Symbol, A)) => (Symbol, A)): Map[Symbol, PValue] = {
      map.map {
        case (k, PMap(innerMap)) => k -> PValue.fromMap(modify(innerMap, f))
        case (k, v) =>
          decoder(v)
            .toOption
            .flatMap(a => if (p(k -> a)) Some(f(k -> a)) else None)
            .map { case (k2, a2) => k2 -> encoder(a2) }
            .getOrElse(k -> v)
      }
    }

    TraversalPath(Traversal[PValue, (Symbol, A)] {
      case PValue.PMap(map) => getAll(map)
      case _ => Seq.empty
    } { f =>
      s =>
        getter.get(s).flatMap {
          case PMap(source) => PResult.valid(PValue.fromMap(modify(source, f)))
          case other => PResult.invalid(TypeCastFailure(s"Expected PMap value but got $other"))
        }
    })
  }

  /** Try fallback option on failure */
  def orElse(fallback: GetterPath): GetterPath = GetterPath(s =>
    getter.get(s).orElse(fallback.getter.get(s))
  )

  /** all dot field magic is here */
  def selectDynamic(field: String): GetterPath = LensPath(mapLens.compose(index(Symbol(field))))
}

object GetterPath {
  def apply(get: PValue => PResult[PValue]) = new GetterPath(Getter(get))
}