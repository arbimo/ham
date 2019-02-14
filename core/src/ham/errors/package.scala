package ham

import cats.data.NonEmptyList

package object errors {

//  type Attempt[+A] = Either[Err, A]

  /** Alias for the companion objects of Attempt cases. */
  val Succ: Attempt.Succ.type = Attempt.Succ
  val Fail: Attempt.Fail.type = Attempt.Fail
  /* Alternative formulation that hides implementation details. Might be useful later to remove boxing
  object Succ {
    def apply[A](a: A): Attempt[A] = Attempt.Succ(a)
    def unapply[A](fa: Attempt[A]): Option[A] = fa.toOption
  }
  object Fail {
    def apply(a: Err): Attempt[Nothing] = Attempt.Fail(a)
    def unapply[A](fa: Attempt[A]): Option[Err] = fa match {
      case Attempt.Fail(err) => Some(err)
      case Attempt.Succ(_) => None
    }
  }
   */

  def error(msg: String, cause: Throwable = null): Err = new Err(msg, cause = cause)
  def combined(err1: Err, err2: Err, others: Err*): Err =
    new MultipleErr(NonEmptyList(err1, err2 :: others.toList))
  def failure(msg: String, cause: Throwable = null): Attempt[Nothing] = Fail(error(msg, cause))
  def success[A](a: A): Attempt[A]                                    = Succ(a)

  implicit class OptionOps[A](private val v: Option[A]) extends AnyVal {
    def toAttempt(err: => Err): Attempt[A] = Attempt.fromOption(v, err)
  }
  implicit class EitherOps[A](private val v: Either[Err, A]) extends AnyVal {
    def toAttempt: Attempt[A] = Attempt.fromEither(v)
  }

}
