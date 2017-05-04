package io.parsek.optics
import cats.syntax.either._

/**
  * @author andr83
  */
case class PValidation[S, T, +E, A, B](private
  val _getOrModify: S => (E,T) Either A)(
  val _set: B => S => T
) { self=>
  @inline final def set(b: B): (S) => T = _set(b)

  @inline final def get(s: S): E Either A = _getOrModify(s).fold[E Either A](e=> Left(e._1), Right.apply)

  @inline final def getOption(s: S): Option[A] = _getOrModify(s).toOption

  @inline final def modify(f: A => B): S => T =
    s => _getOrModify(s).fold(e=> e._2, a=> _set(f(a))(s))

  @inline final def modifyValidate(f: A => B): S => Either[E, T] =
    s => _getOrModify(s).fold(e=> Left(e._1), a=> Right(_set(f(a))(s)))

  @inline final def modifyOption(f: A => B): S => Option[T] =
    s=> modifyValidate(f)(s).toOption

  @inline final def setValidate(b: B): S => E Either T = modifyValidate(_ => b)

  @inline final def setOption(b: B): S => Option[T] = modifyOption(_ => b)

  @inline final def compose[C, D, F >: E](other: PValidation[A, B, F, C, D]): PValidation[S, T, E, C, D] =
    new PValidation[S, T, E, C, D](
      s => self._getOrModify(s).flatMap(a=> other._getOrModify(a).bimap({case (f, b)=> f.asInstanceOf[E] -> self._set(b)(s)}, identity)))(
      d => s=> self.modify(other.set(d))(s)
    )

  //def orElse[C, D, F >: E](other: PValidation[A, B, F, C, D]): PValidation[S, T, E, C, D]
  //@inline final def compose[C, D](other: PPrism[A, B, C, D]): PValidation[S, T, C, D] = compose(other.asOptional)
}

object Validation {
  def apply[S, E, A](getValidate: S => E Either A)(set: A => S => S): Validation[S, E, A] = new PValidation[S, S, E, A, A](
    s => getValidate(s).fold[(E, S) Either A](e=> Left(e -> s), Right.apply))(
    a => s => set(a)(s)
  )

  def id[A, E]: Validation[A, E, A] = new PValidation[A, A, E, A, A](
    s=> Right(s))(
    _=> identity
  )
}
