package io.parsek

/**
  * Source: https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/data/NonEmptyList.scala
  *
  * A data type which represents a non empty list of A, with
  * single element (head) and optional structure (tail).
  */
final case class NonEmptyList[+A](head: A, tail: List[A]) {

  /**
    * Return the head and tail into a single list
    * {{{
    * scala> import cats.data.NonEmptyList
    * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
    * scala> nel.toList
    * res0: scala.collection.immutable.List[Int] = List(1, 2, 3, 4, 5)
    * }}}
    */
  def toList: List[A] = head :: tail

  /**
    * Selects the last element
    * {{{
    * scala> import cats.data.NonEmptyList
    * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
    * scala> nel.last
    * res0: Int = 5
    * }}}
    */
  def last: A = tail.lastOption match {
    case None    => head
    case Some(a) => a
  }

  /**
    * The size of this NonEmptyList
    *
    * {{{
    * scala> import cats.data.NonEmptyList
    * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
    * scala> nel.size
    * res0: Int = 5
    * }}}
    */
  def size: Int = 1 + tail.size

  def length: Int = size

  /**
    *  Applies f to all the elements of the structure
    */
  def map[B](f: A => B): NonEmptyList[B] =
    NonEmptyList(f(head), tail.map(f))

  def ++[AA >: A](l: List[AA]): NonEmptyList[AA] =
    concat(l)

  def concat[AA >: A](other: List[AA]): NonEmptyList[AA] =
    NonEmptyList(head, tail ::: other)

  /**
    * Append another NonEmptyList
    */
  def concatNel[AA >: A](other: NonEmptyList[AA]): NonEmptyList[AA] =
    NonEmptyList(head, tail ::: other.toList)

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] =
    f(head) ++ tail.flatMap(f andThen (_.toList))

  def ::[AA >: A](a: AA): NonEmptyList[AA] =
    prepend(a)

  def prepend[AA >: A](a: AA): NonEmptyList[AA] =
    NonEmptyList(a, head :: tail)

  /**
    * Alias for concatNel
    *
    * {{{
    * scala> import cats.data.NonEmptyList
    * scala> val nel = NonEmptyList.of(1, 2, 3)
    * scala> nel ::: NonEmptyList.of(4, 5)
    * res0: cats.data.NonEmptyList[Int] = NonEmptyList(1, 2, 3, 4, 5)
    * }}}
    */
  def :::[AA >: A](other: NonEmptyList[AA]): NonEmptyList[AA] =
    other.concatNel(this)

  /**
    * Remove elements not matching the predicate
    *
    * {{{
    * scala> import cats.data.NonEmptyList
    * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
    * scala> nel.filter(_ < 3)
    * res0: scala.collection.immutable.List[Int] = List(1, 2)
    * }}}
    */
  def filter(p: A => Boolean): List[A] = {
    val ftail = tail.filter(p)
    if (p(head)) head :: ftail
    else ftail
  }

  /**
    * Remove elements matching the predicate
    *
    * {{{
    * scala> import cats.data.NonEmptyList
    * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
    * scala> nel.filterNot(_ < 3)
    * res0: scala.collection.immutable.List[Int] = List(3, 4, 5)
    * }}}
    */
  def filterNot(p: A => Boolean): List[A] = {
    val ftail = tail.filterNot(p)
    if (p(head)) ftail
    else head :: ftail
  }

  /**
    * Builds a new `List` by applying a partial function to
    * all the elements from this `NonEmptyList` on which the function is defined
    *
    * {{{
    * scala> import cats.data.NonEmptyList
    * scala> val nel = NonEmptyList.of(1, 2, 3, 4, 5)
    * scala> nel.collect { case v if v < 3 => v }
    * res0: scala.collection.immutable.List[Int] = List(1, 2)
    * scala> nel.collect {
    *      |  case v if v % 2 == 0 => "even"
    *      |  case _ => "odd"
    *      | }
    * res1: scala.collection.immutable.List[String] = List(odd, even, odd, even, odd)
    * }}}
    */
  def collect[B](pf: PartialFunction[A, B]): List[B] = {
    if (pf.isDefinedAt(head)) {
      pf.apply(head) :: tail.collect(pf)
    } else {
      tail.collect(pf)
    }
  }

  /**
    * Find the first element matching the predicate, if one exists
    */
  def find(p: A => Boolean): Option[A] =
    if (p(head)) Some(head)
    else tail.find(p)

  /**
    * Check whether at least one element satisfies the predicate
    */
  def exists(p: A => Boolean): Boolean =
    p(head) || tail.exists(p)

  /**
    * Check whether all elements satisfy the predicate
    */
  def forall(p: A => Boolean): Boolean =
    p(head) && tail.forall(p)

  /**
    * Left-associative fold on the structure using f.
    */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    tail.foldLeft(f(b, head))(f)

  override def toString: String = s"NonEmpty$toList"
}

object NonEmptyList {
  def of[A](head: A, tail: A*): NonEmptyList[A] =
    NonEmptyList(head, tail.toList)

  def one[A](head: A): NonEmptyList[A] = NonEmptyList(head, Nil)

  /**
    * Create a `NonEmptyList` from a `List`, or throw an
    * `IllegalArgumentException` if the input list is empty.
    *
    * is empty.
    */
  def fromListUnsafe[A](l: List[A]): NonEmptyList[A] =
    l match {
      case Nil =>
        throw new IllegalArgumentException(
          "Cannot create NonEmptyList from empty list")
      case h :: t => NonEmptyList(h, t)
    }
}
