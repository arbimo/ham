package ham

package object errors {

  type Attempt[+A] = Either[Err, A]

  def error(msg: String, cause: Throwable = null): Err = new Err(msg)
  def failure(msg: String, cause: Throwable = null): Attempt[Nothing] = Left(error(msg, cause))
  def success[A](a: A): Attempt[A] = Right(a)

  implicit class EitherOps[A](private val attempt: Attempt[A]) extends AnyVal {

  }

//  implicit class Ops[A](private val l: List[A]) extends AnyVal {
//    def fold[B](f : A => Attempt[B]): Attempt[List[B]] = {
//      def go(l: List[A]): Attempt[List[A]] =
//    }
//  }

}
