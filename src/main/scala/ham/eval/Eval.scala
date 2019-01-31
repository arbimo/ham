package ham.eval
import ham.expr.IExpr



object Eval {

  import ham.expr.IExpr._

  def evaluator[Ctx, E: ham.expr.IExpr](e: E, ofSym: String => Option[Either[Ctx => Any, E]]): Ctx => Any = e match {
    case ham.expr.IExpr.Cst(v) => (_: Ctx) => v
    case ham.expr.IExpr.Sym(name) => ofSym(name) match {
      case Some(Left(f)) => f
      case Some(Right(e2)) => evaluator(e2, ofSym)
      case None => sys.error(s"Unknown symbol: $name")
    }
    case ham.expr.IExpr.App(fun, arg) =>
      val funPE = evaluator(fun, ofSym).asInstanceOf[Ctx => Any => Any]
      val argPE = evaluator(arg, ofSym)
      (s: Ctx) => funPE(s)(argPE(s))
  }




  object AsError  {
    import ham.model._
    private var i = 0
    def nextSym(): Sym = {i += 1; Sym("_a"+i) }

    val self = Sym("_as_error")

//    override def apply(e: AST): AST = e match {
//      case c @ Num(v) => c
//      case App(fun, arg) => App(App(self, fun), arg)
//      case Sym("&") => "fun a b -> $self(a) + $self(b)"
//        val a = nextSym()
//        val b = nextSym()
//        Lambda(a, Lambda(b, App(App(Sym("+"), App(self, a)), App(self, b))))
//    }
    // e & = \a -> \b -> (e a) + (e b)
    // e < = \a -> \b -> max(0, abs(a - b))
    // e (f a) = ((e f) a)
    // e b = if b then 0 else inf
  }

//  def macroExpand[E: ham.expr.Expr](e: E, lookup: String => Option[Either[Macro[E], E]]): E = e match {
//    case ham.expr.Expr.Cst(v) => v
//    case ham.expr.Expr.Sym(name) => ofSym(name) match {
//      case Some(Left(f)) => f
//      case Some(Right(e2)) => evaluator(e2, ofSym)
//      case None => sys.error(s"Unknown symbol: $name")
//    }
//    case ham.expr.Expr.App(fun, arg) =>
//      val funPE = evaluator(fun, ofSym).asInstanceOf[Ctx => Any => Any]
//      val argPE = evaluator(arg, ofSym)
//      (s: Ctx) => funPE(s)(argPE(s))
//  }

}

