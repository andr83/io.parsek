package io.parsek.optics
import cats.syntax.either._

/**
  * @author andr83
  */
case class POptional[S, T, A, B](private
  val _getOrModify: S => T Either A)(
  val _set: B => S => T
) { self=>
  @inline final def set(b: B): (S) => T = _set(b)

  @inline final def getOption(s: S): Option[A] = _getOrModify(s).toOption

  @inline final def modify(f: A => B): S => T =
    s=> _getOrModify(s).fold(identity, a=> _set(f(a))(s))

  @inline final def modifyOption(f: A => B): S => Option[T] =
    s=> getOption(s).map(a=> _set(f(a))(s))

  @inline final def setOption(b: B): S => Option[T] = modifyOption(_ => b)

  @inline final def compose[C, D](other: POptional[A, B, C, D]): POptional[S, T, C, D] = new POptional[S, T, C, D](
    s => self._getOrModify(s).flatMap(a=> other._getOrModify(a).bimap(b=> self._set(b)(s), identity)))(
    d => s=> self.modify(other.set(d))(s)
  )

  @inline final def compose[C, D](other: PPrism[A, B, C, D]): POptional[S, T, C, D] = compose(other.asOptional)
}

object Optional {
  def apply[S, A](getOption: S => Option[A])(set: A => S => S): Optional[S, A] = new POptional[S, S, A, A](
    s=> getOption(s).fold[S Either A](Left(s))(Right.apply))(
    a=>set(a)
  )

  def id[A]: Optional[A, A] = new POptional[A, A, A, A](
    s=> Right(s))(
    _=> identity
  )
}
