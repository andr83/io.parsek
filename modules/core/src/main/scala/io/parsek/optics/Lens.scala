package io.parsek.optics

import io.parsek.PResult

/**
  * @author Andrei Tupitcyn
  *
  *         [[Lens]] is:
  *         {{{
  *          case class User(name: String, id: String)
  *
  *          val nameLens = Lens[User, User, String, String](user=> PResult.valid(user.name), newName => user => user.name = newName)
  *
  *          val idLens = Lens[User, User, String, Int](user=> PResult.valid(user.id), intId => user => user.id = id.toString)
  *
  *          val userAndrei = User("Andrei", "10")
  *          val nameResult: PResult[String] = nameLens.get(userAndrei)
  *          // PSuccess("Andrei")
  *
  *          val userAndrei2 = idLens.set(2)(userAndrei)
  *          // User("Andrei", "2")
  *
  *          val userBulat = nameLens.set("Bulat")(userAndrei)
  *          // User("Bulat", "10")
  *         }}}
  * @tparam S The source value type on which we apply lens
  * @tparam T The modified source type after applying lens
  * @tparam A The target type
  * @tparam B The modified target type
  */
abstract class PLens[S, T, A, B] extends Getter[S, A] {
  self =>
  def get(s: S): PResult[A]

  def set(b: B)(s: S): PResult[T]

  def tryModify(f: A => PResult[B]): S => PResult[T]

  def modify(f: A => B): S => PResult[T] = tryModify(a => PResult.catchNonFatal(f(a)))

  @inline final def compose[C, D](other: PLens[A, B, C, D]): PLens[S, T, C, D] =
    new PLens[S, T, C, D] {
      def get(s: S): PResult[C] = self.get(s).flatMap(a => other.get(a))

      def set(d: D)(s: S): PResult[T] = self.tryModify(other.set(d))(s)

      def tryModify(f: (C => PResult[D])): (S => PResult[T]) = self.tryModify(other.tryModify(f))
    }
}

object Lens {
  def apply[S, A](_get: S => PResult[A])(_set: A => S => PResult[S]): PLens[S, S, A, A] = new PLens[S, S, A, A] {
    override def get(s: S): PResult[A] = _get(s)

    override def set(b: A)(s: S): PResult[S] = _set(b)(s)

    override def tryModify(f: (A) => PResult[A]): (S) => PResult[S] =
      s => for (
        a1 <- _get(s);
        a2 <- f(a1);
        s2 <- _set(a2)(s)
      ) yield s2
  }

  def id[A]: PLens[A, A, A, A] = new PLens[A, A, A, A] {
    override def get(s: A): PResult[A] = PResult.valid(s)

    override def set(b: A)(s: A): PResult[A] = PResult.valid(b)

    override def tryModify(f: (A) => PResult[A]): (A) => PResult[A] = f
  }
}
