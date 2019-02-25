package hydra
import cats.Applicative
import _root_.algebra.ring.Field
import com.stripe.rainier.compute.Real

package object algebra {

  type Num[A] = Field[A]
  object Num {
    def apply[A](implicit ev: Num[A]): Num[A] = ev
  }

  implicit val realField: Field[Real] = new Field[Real] {
    override def fromDouble(a: Double): Real   = Real(a)
    override def negate(x: Real): Real         = Real.zero - x
    override def times(x: Real, y: Real): Real = x * y
    override def zero: Real                    = Real.zero
    override def plus(x: Real, y: Real): Real  = x + y
    override def div(x: Real, y: Real): Real   = x / y
    override def one: Real                     = Real.one
  }

  implicit def funField[A, B](implicit F: Field[B]): Field[A => B] = new Field[A => B] {
    override def fromDouble(a: Double): A => B       = _ => F.fromDouble(a)
    override def negate(x: A => B): A => B           = a => F.negate(x(a))
    override def times(x: A => B, y: A => B): A => B = a => F.times(x(a), y(a))
    override def zero: A => B                        = _ => F.zero
    override def plus(x: A => B, y: A => B): A => B  = a => F.plus(x(a), y(a))
    override def div(x: A => B, y: A => B): A => B   = a => F.div(x(a), y(a))
    override def one: A => B                         = _ => F.one
  }

  implicit def appField[F[_], A](implicit F: Field[A], A: Applicative[F]): Field[F[A]] =
    new Field[F[A]] {
      override def fromDouble(a: Double): F[A]   = A.pure(F.fromDouble(a))
      override def negate(x: F[A]): F[A]         = A.map(x)(F.negate)
      override def times(x: F[A], y: F[A]): F[A] = A.map2(x, y)(F.times)
      override def zero: F[A]                    = A.pure(F.zero)
      override def plus(x: F[A], y: F[A]): F[A]  = A.map2(x, y)(F.plus)
      override def div(x: F[A], y: F[A]): F[A]   = A.map2(x, y)(F.div)
      override def one: F[A]                     = A.pure(F.one)
    }

  implicit class NumOps[A](a: A)(implicit N: Num[A]) {
    def +(b: A): A = N.plus(a, b)
    def -(b: A): A = N.minus(a, b)
    def *(b: A): A = N.times(a, b)
  }

}
