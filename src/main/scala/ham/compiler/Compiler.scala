package ham.compiler

import ham.expr._

class Compiler {

//
//    def evaluator[Ctx](e: Expr, ofSym: Id => Option[Either[Any, Expr]]): Any = e match {
//      case Literal(v, _) => v
//      case
//      case ham.expr.IExpr.Cst(v) => (_: Ctx) => v
//      case ham.expr.IExpr.Sym(name) => ofSym(name) match {
//        case Some(Left(f)) => f
//        case Some(Right(e2)) => evaluator(e2, ofSym)
//        case None => sys.error(s"Unknown symbol: $name")
//      }
//      case ham.expr.IExpr.App(fun, arg) =>
//        val funPE = evaluator(fun, ofSym).asInstanceOf[Ctx => Any => Any]
//        val argPE = evaluator(arg, ofSym)
//        (s: Ctx) => funPE(s)(argPE(s))
//    }
}