package io.parsek.optics
import cats.syntax.either._

/**
  * @author andr83
  */
case class PPrism[S, T, A, B](
  _getOrModify: S => T Either A)(
  _reverseGet: B => T
) extends Serializable {self=>
  @inline final def reverseGet(b: B): T = reverseGet(b)

  @inline final def getOption(s: S): Option[A] = _getOrModify(s).toOption

  @inline final def modify(f: A => B): S => T =
    s => _getOrModify(s).fold(identity, a=> _reverseGet(f(a)))

  @inline final def modifyOption(f: A => B): S => Option[T] =
    s => getOption(s).map(a=> _reverseGet(f(a)))

  @inline final def set(b: B): S => T = modify(_ => b)

  @inline final def setOption(b: B): S => Option[T] = modifyOption(_ => b)

  @inline final def compose[C, D](other: PPrism[A, B, C, D]): PPrism[S, T, C, D] = new PPrism[S, T, C, D](
    s=> self._getOrModify(s).flatMap(a=> other._getOrModify(a).bimap(b=> self.set(b)(s), identity)))(
    d=> self._reverseGet(other.reverseGet(d))
  )

  //@inline final def compose[C,D](other: PValidation[A, B, C, D]): PValidation[S, T, C, D] = self.asOptional compose other

  //@inline final def asOptional: PValidation[S, T, A, B] = new PValidation[S, T, A, B](self._getOrModify)(self.set)
}

object Prism {
  def apply[S, A](getOption: S => Option[A])(reverseGet: A => S): Prism[S, A] = new PPrism[S, S, A, A](s => {
    getOption(s).fold[S Either A](Left(s))(a=> Right(a))
  })(reverseGet)

  def partial[S,A](get: PartialFunction[S, A])(reverseGet: A => S): Prism[S, A] = Prism[S, A](get.lift)(reverseGet)

  def partialOption[S,A](getOption: PartialFunction[S, Option[A]])(reverseGet: A => S): Prism[S, A] =
    Prism[S, A](s=> getOption.lift(s).flatten)(reverseGet)
}