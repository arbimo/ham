package hydra.compile

import com.stripe.rainier.compute._
import ham.expr.{BuiltIn, Expr, Id}
import ham.prelude.Prelude

object Rainier {

  def compile(e: Expr, ofSym: Id => Option[Either[Real, Expr]]) = ham.errors.attempt {
    compileUnsafe(e, ofSym, Nil)
  }

  def compileUnsafe(e: Expr, ofSym: Id => Option[Either[Real, Expr]], stack: List[Real]): Real =
    e match {
      case ham.expr.Literal(x: Double, Prelude.Real) => Real(x)
      case ham.expr.Fun(Nil, body)                   => compileUnsafe(body, ofSym, stack)
      case ham.expr.Fun(_, body)                     => ???
      case ham.expr.Var(_)                           => ???
      case ham.expr.Symbol(id) =>
        ofSym(id) match {
          case None           => throw ham.errors.error(s"Unknown symbol: $id")
          case Some(Left(f))  => f
          case Some(Right(e)) => compileUnsafe(e, ofSym, stack)
        }
      case ham.expr.App(fun, arg) =>
        // place arg on stack
        val argPE = compileUnsafe(arg, ofSym, stack)
        compileUnsafe(fun, ofSym, argPE :: stack)

      case BuiltIn("real.add", _) => stack(0) + stack(1)
      case BuiltIn("real.sub", _) => stack(0) - stack(1)
      case BuiltIn("real.mul", _) => stack(0) * stack(1)
      case BuiltIn("real.div", _) => stack(0) / stack(1)
      case BuiltIn("real.max", _) => stack(0) max stack(1)
      case BuiltIn("real.min", _) => stack(0) min stack(1)
      case BuiltIn("real.abs", _) => stack(0).abs

    }

  def compile(params: IndexedSeq[Variable], f: Real): FunN = {
    val compiler = Compiler.default
    val compiled = compiler.compile(params, f)
    FunN.byNumericDiff(params.size, compiled)
  }

}
