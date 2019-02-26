package hydra.reductions

import ham.expr._
import ham.prelude.Prelude._

import scala.annotation.tailrec

object Reductions {

  implicit class ExprOps(private val lhs: Expr) extends AnyVal {
    def apply(args: Expr*): Expr = args.foldLeft(lhs)(App(_, _))
  }

  def expand(e: Expr, stack: List[Expr], env: Id => Option[Expr]): Expr = e match {
    case x: Literal       => x
    case Var(i)           => stack(i)
    case sym @ Symbol(id) => env(id).map(expand(_, stack, env)).getOrElse(sym)
    case Fun(_, f)        => expand(f, stack, env)
    case App(f, a)        => expand(f, a :: stack, env)
    case x: BuiltIn =>
      x.apply(stack.take(x.tpe.arity): _*)
  }

  def bool2err(e: Expr, stack: List[Expr], env: Id => Expr): Expr = {
    e match {
      case Literal(true, Bool)  => Literal(0.0, Real)
      case Literal(false, Bool) => Literal(Double.PositiveInfinity, Real)
      case Var(i)               => stack(i)
      case Symbol(id)           => bool2err(env(id), stack, env)
      case Fun(_, f)            => bool2err(f, stack, env)
      case App(f, a)            => bool2err(f, a :: stack, env)
      case BuiltIn("bool.eq", _) =>
        val a = stack(0)
        val b = stack(1)
        unsafeBuiltIn("-")(a, b)
      case BuiltIn("real.lt", _) =>
        val lhs = stack(0)
        val rhs = stack(1)
        // retract small epsilon to difference
        val diff = unsafeBuiltIn("-")(unsafeBuiltIn("-")(lhs, rhs), Literal(1e-8, Real))
        unsafeBuiltIn("max")(
          diff,
          Literal(0.0, Real)
        )
      case BuiltIn("real.leq", _) =>
        val lhs = stack(0)
        val rhs = stack(1)
        unsafeBuiltIn("max")(unsafeBuiltIn("-")(lhs, rhs), Literal(0.0, Real))
      case BuiltIn("real.geq", _) =>
        val lhs = stack(0)
        val rhs = stack(1)
        unsafeBuiltIn("max")(unsafeBuiltIn("-")(rhs, lhs), Literal(0.0, Real))

      case BuiltIn("bool.and", _) =>
        val lhs :: rhs :: Nil = stack.take(2).map(bool2err(_, stack, env))
        unsafeBuiltIn("max")(lhs, rhs)
      case BuiltIn("bool.or", _) =>
        val lhs :: rhs :: Nil = stack.take(2).map(bool2err(_, stack, env))
        unsafeBuiltIn("min")(lhs, rhs)

    }
  }

}
