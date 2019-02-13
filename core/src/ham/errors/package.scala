package ham

import cats.data.NonEmptyList

package object errors {

  type Attempt[+A] = Either[Err, A]

  def error(msg: String, cause: Throwable = null): Err = new Err(msg, cause = cause)
  def combined(err1: Err, err2: Err, others: Err*): Err = new MultipleErr(NonEmptyList(err1, err2 :: others.toList))
  def failure(msg: String, cause: Throwable = null): Attempt[Nothing] = Left(error(msg, cause))
  def success[A](a: A): Attempt[A] = Right(a)

  def assertSucceedsTo[A](attempt: Attempt[A], expected: A): Unit = {
    attempt match {
      case Right(a) => assert(a == expected)
      case Left(err) =>
        System.err.println(err)
        err.printStackTrace()
        throw new AssertionError(s"Computation failed")
    }
  }
  def assertSucceeds[A](attempt: Attempt[A]): Unit = {
    attempt match {
      case Right(a) =>
      case Left(err) =>
        System.err.println(err)
        err.printStackTrace()
        throw new AssertionError(s"Computation failed")
    }
  }
  def assertFails[A](attempt: Attempt[A]): Unit =
    attempt match {
      case Left(_) =>
      case Right(v) => throw new AssertionError(s"Expected failure but got: $v")
    }

  implicit class EitherOps[A](private val attempt: Attempt[A]) extends AnyVal {

  }

//  implicit class Ops[A](private val l: List[A]) extends AnyVal {
//    def fold[B](f : A => Attempt[B]): Attempt[List[B]] = {
//      def go(l: List[A]): Attempt[List[A]] =
//    }
//  }

}
