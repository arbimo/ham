package ham.errors

import scala.language.higherKinds

import cats.{Applicative, Eval, Monad, MonadError, Traverse}

import scala.annotation.tailrec

sealed abstract class Attempt[+A] {

  def map[B](f: A => B): Attempt[B] = this match {
    case Attempt.Succ(a) => Attempt.Succ(f(a))
    case x: Attempt.Fail => x
  }

  def foreach(f: A => Unit): Unit = this match {
    case Attempt.Succ(a) => f(a)
    case _               =>
  }

  def orElse[B >: A](alt: => Attempt[B]): Attempt[B] = this match {
    case succ: Attempt.Succ[A] => succ
    case _                     => alt
  }

  def toEither: Either[Err, A] = this match {
    case Attempt.Succ(v) => Right(v)
    case Attempt.Fail(e) => Left(e)
  }
  def toOption: Option[A] = this match {
    case Attempt.Succ(v) => Some(v)
    case Attempt.Fail(_) => None
  }

  /** Unsafe methods for testing purposes */
  def assertSucceedsTo[B >: A](expected: B): Unit = this match {
    case Succ(a) =>
      if(a != expected)
        throw new AssertionError(s"Expected x1 but got x2:\nx1 = $expected\nx2 = $a")
    case Fail(err) =>
      System.err.println(err)
      err.printStackTrace()
      throw new AssertionError(s"Computation failed")
  }

  def assertSucceeds: Unit = this match {
    case Succ(_) =>
    case Fail(err) =>
      System.err.println(err)
      err.printStackTrace()
      throw new AssertionError(s"Computation failed")
  }

  def assertFails: Unit = this match {
    case Fail(_) =>
    case Succ(v) => throw new AssertionError(s"Expected failure but got: $v")
  }

  def unsafeGet: A = this match {
    case Succ(a)   => a
    case Fail(err) => throw err
  }
}

object Attempt {

  private[errors] final case class Succ[+A](v: A) extends Attempt[A]
  private[errors] final case class Fail(e: Err)   extends Attempt[Nothing]

  def fromEither[A](either: Either[Err, A]): Attempt[A] = either match {
    case Right(a)  => Succ(a)
    case Left(err) => Fail(err)
  }
  def fromOption[A](opt: Option[A], err: => Err): Attempt[A] = opt match {
    case Some(a) => Succ(a)
    case None    => Fail(err)
  }

  implicit val monadError: Monad[Attempt] = new MonadError[Attempt, Err] with Traverse[Attempt] {
    override def pure[A](x: A): Attempt[A] = Attempt.Succ(x)

    override def flatMap[A, B](fa: Attempt[A])(f: A => Attempt[B]): Attempt[B] = fa match {
      case Succ(v) => f(v)
      case x: Fail => x
    }

    @tailrec override def tailRecM[A, B](a: A)(f: A => Attempt[Either[A, B]]): Attempt[B] =
      f(a) match {
        case Succ(Right(b)) => Succ(b)
        case Succ(Left(a2)) => tailRecM(a2)(f)
        case x: Fail        => x
      }

    override def raiseError[A](e: Err): Attempt[A] = Fail(e)

    override def handleErrorWith[A](fa: Attempt[A])(f: Err => Attempt[A]): Attempt[A] = fa match {
      case succ: Succ[A] => succ
      case Fail(e)       => f(e)
    }

    override def traverse[G[_], A, B](fa: Attempt[A])(f: A => G[B])(
        implicit G: Applicative[G]): G[Attempt[B]] =
      fa match {
        case Succ(a) => G.map(f(a))(Succ(_))
        case Fail(e) => G.pure(Fail(e))
      }

    override def foldLeft[A, B](fa: Attempt[A], b: B)(f: (B, A) => B): B = fa match {
      case Succ(a) => f(b, a)
      case Fail(_) => b
    }

    override def foldRight[A, B](fa: Attempt[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa match {
        case Succ(a) => f(a, lb)
        case Fail(_) => lb
      }
  }

}
