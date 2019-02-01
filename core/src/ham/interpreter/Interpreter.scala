package ham.interpreter

import ham.errors.Attempt
import ham.eval.Functions
import ham.expr._

import scala.util.{Failure, Success, Try}

object Interpreter {

  type Stack = List[Expr]

  def eval(e: Expr, lookup: Id => Attempt[Expr]): Attempt[Expr] = {
    val unsafeLookup: Id => Expr = id => {
      lookup(id) match {
        case Right(e) => e
        case Left(err) => throw err
      }
    }
    Try(eval(e, Nil, unsafeLookup)) match {
      case Success(res) => ham.errors.success(res)
      case Failure(err: ham.errors.Err) => Left(err)
      case Failure(e) => throw e // this a crash do not pretend we know how to handle it
    }

  }

  private def eval(e: Expr, stack: Stack, lookup: Id => Expr): Expr = {
    val res = e match {
      case App(l, r) =>
        eval(l, r :: stack, lookup)
      case Fun(args, body) if args.length <= stack.size =>
        eval(instantiate(body, stack), stack.drop(args.length), lookup)
      case f :Fun =>
        rebuild(f, stack)
      case Symbol(id) =>
        eval(lookup(id), stack, lookup)
      case BuiltIn(name, t) =>
        var arity = t.arity
        var func = Functions
          .apply(name)
          .getOrElse(throw ham.errors.error(s"undefined builtin function $name"))
        var s = stack
        while(arity > 0) {
          val arg = s.head
          val evaluatedArg = eval(arg, stack, lookup) match {
            case Literal(v, _) => v
            case _ => throw ham.errors.error(s"an argument of built in did not evaluate to a literal")
          }

          s = stack.tail
          arity -= 1
          func = func.asInstanceOf[Any => Any].apply(evaluatedArg)
        }
        Literal(func, null) // todo: do we really need the type in literals
      case x: Literal =>
        x
    }
    res
  }

  def instantiate(e: Expr, stack: Stack): Expr = e match {
    case App(l, r) => App(instantiate(l, stack), instantiate(r, stack))
    case Var(n) => stack(n)
    case x => x
  }

  def rebuild(e: Expr, stack: Stack): Expr = stack match {
    case Nil => e
    case x :: xs => rebuild(App(e, x), xs)
  }
}