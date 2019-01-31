package ham.interpreter

import ham.expr.IExpr

import ham.expr.IExpr._




class Partial[E: IExpr] {


  type Stack = List[E]

//  def eval(e: E, funs: IndexedSeq[E]): E = eval(e, Nil, funs)
//
//   def eval(e: E, stack: Stack, lookup: String => Option[Either[Any, E]]): E = {
//    val res = e match {
//      case App(l, r) =>
//        eval(l, r :: stack, lookup)
//      case Sym(name) => lookup(name) match {
//        case Some(Left(value)) => Expr[E].makeConstant(value)
//        case Some(Right(expr)) => expr
//        case None => sys.error(s"lookup failed for: $name")
//      }
//
//      case Fun(na, fn, _) if na <= stack.size =>
//        eval(instantiate(funs(fn), stack), stack.drop(na), funs)
//      case f@Fun(na, fn, _) =>
//        rebuild(f, stack)
//    }
////     println("  " + stack)
////    println(s"  $res <- $e")
//
//    res
//  }
//
//  def instantiate(e: E, stack: Stack): E = e match {
//    case App(l, r) => App(instantiate(l, stack), instantiate(r, stack))
//    case Var(n) => stack(n)
//    case x => x
//  }
//
//  def rebuild(e: E, stack: Stack): E = stack match {
//    case Nil => e
//    case x :: xs => rebuild(App(e, x), xs)
//  }
}