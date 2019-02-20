package ham.typing

import ham.errors.{Attempt, Fail, Succ}
import ham.expr
import ham.expr._
import ham.expr.Type
import scala.collection.mutable

object Typer {

  class Err(msg: String) extends ham.errors.Err(msg)
  private def parseError(msg: String) = new Err(s"Parse error: $msg")
  private def typeError(msg: String)  = new Err(s"Type error: $msg")
  def error(msg: String): Err         = new Err(msg)

  def typeOf(expr: Expr, ctx: Id => Attempt[Type]): Attempt[Type] =
    try {
      val checker = new Checker
      Succ(checker.analyse(expr, Env(ctx, Nil), Set.empty))
    } catch {
      case e: Err => ham.errors.failure(s"Failed to type check $expr\nCause: ${e.msg}", cause = e)
    }

  private case class Env(ctx: Id => Attempt[Type], stack: List[Type]) {
    def apply(k: Id): Attempt[Type] = ctx(k)
    def push(tpe: Type): Env        = Env(ctx, tpe :: stack)
  }

  private class MutVarBindings(map: mutable.Map[Type.Var, Type]) {
    def apply(v: Type.Var): Option[Type]                         = map.get(v)
    def set(v: Type.Var, tpe: Type): Unit                        = map.update(v, tpe)
    def getOrElseUpdate(v: Type.Var, default: => Type.Var): Type = map.getOrElseUpdate(v, default)
  }

  private class Checker() {
    val bindings: MutVarBindings = new MutVarBindings(mutable.Map())

    def analyse(node: Expr, env: Env, nongen: Set[Type.Var]): Type = {
      val res = analyseNoSimplify(node, env, nongen)
      replaceVars(res)
    }
    private def replaceVars(tpe: Type): Type = {
      tpe match {
        case Type.Oper(name, args) => Type.Oper(name, args.map(replaceVars))
        case v: Type.Var =>
          bindings.getOrElseUpdate(v, new Type.Var)
      }
    }

    private def analyseNoSimplify(node: Expr, env: Env, nongen: Set[Type.Var]): Type =
      node match {
        case Literal(_, tpe) => tpe
        case BuiltIn(_, tpe) => tpe
        case Var(pos)        => env.stack(pos) // todo: handle error
        case Symbol(id)      => getType(id, env, nongen)
        case App(fun, arg) =>
          val funType    = analyseNoSimplify(fun, env, nongen)
          val argType    = analyseNoSimplify(arg, env, nongen)
          val resultType = new expr.Type.Var
          unify(Type.function(argType, resultType), funType)
          resultType
        case Fun(Nil, body) =>
          analyseNoSimplify(body, env, nongen)
        case Fun(first :: rest, body) =>
          val firstArgType = new Type.Var
          val curried      = Fun(rest, body)
          val nextEnv      = env.push(firstArgType)
          val resultType   = analyseNoSimplify(curried, nextEnv, nongen + firstArgType)
          Type.function(firstArgType, resultType)
      }

    private def unify(t1: Type, t2: Type) {
      val type1 = prune(t1)
      val type2 = prune(t2)
      (type1, type2) match {
        case (a: Type.Var, b) =>
          if(a != b) {
            if(occursintype(a, b))
              typeError("recursive unification")
            bindings.set(a, b)
          }
        case (a: Type.Oper, b: Type.Var) => unify(b, a)
        case (a: Type.Oper, b: Type.Oper) => {
          if(a.name != b.name ||
             a.args.length != b.args.length)
            throw typeError(s"Type mismatch: $a ≠ $b")

          for(i <- a.args.indices)
            unify(a.args(i), b.args(i))
        }
      }
    }

    private def getType(name: Id, env: Env, nongen: Set[Type.Var]): Type = {
      env(name) match {
        case Succ(tpe) => fresh(tpe, nongen)
        case Fail(msg) => throw msg
      }
    }

    // Note: must be called with v 'pre-pruned'
    private def isgeneric(v: Type.Var, nongen: Set[Type.Var]): Boolean = !(occursin(v, nongen))

    private def fresh(t: Type, nongen: Set[Type.Var]): Type = {
      import scala.collection.mutable
      val mappings = new mutable.HashMap[Type.Var, Type.Var]
      def freshrec(tp: Type): Type = {
        prune(tp) match {
          case v: Type.Var =>
            if(isgeneric(v, nongen))
              mappings.getOrElseUpdate(v, new Type.Var)
            else
              v

          case Type.Oper(name, args) =>
            Type.Oper(name, args.map(freshrec(_)))
        }
      }

      freshrec(t)
    }

    // Returns the currently defining instance of t.
    // As a side effect, collapses the list of type instances.
    private def prune(t: Type): Type = t match {
      case v: Type.Var =>
        bindings(v) match {
          case Some(tpe) =>
            val inst = prune(tpe)
            bindings.set(v, inst)
            inst
          case None => t
        }
      case _ => t
    }

    // Note: must be called with v 'pre-pruned'
    private def occursintype(v: Type.Var, type2: Type): Boolean = {
      prune(type2) match {
        case `v`                   => true
        case Type.Oper(name, args) => occursin(v, args)
        case _                     => false
      }
    }

    private def occursin(t: Type.Var, list: Iterable[Type]): Boolean =
      list exists (t2 => occursintype(t, t2))
  }

}
