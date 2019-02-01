package ham.eval



object Eval {


//  def evaluator[Ctx, E: ham.expr.IExpr](e: E, ofSym: String => Option[Either[Ctx => Any, E]]): Ctx => Any = e match {
//    case ham.expr.IExpr.Cst(v) => (_: Ctx) => v
//    case ham.expr.IExpr.Sym(name) => ofSym(name) match {
//      case Some(Left(f)) => f
//      case Some(Right(e2)) => evaluator(e2, ofSym)
//      case None => sys.error(s"Unknown symbol: $name")
//    }
//    case ham.expr.IExpr.App(fun, arg) =>
//      val funPE = evaluator(fun, ofSym).asInstanceOf[Ctx => Any => Any]
//      val argPE = evaluator(arg, ofSym)
//      (s: Ctx) => funPE(s)(argPE(s))
//  }




}

