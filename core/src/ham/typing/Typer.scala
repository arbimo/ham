package ham.typing

import ham.errors.Attempt
import ham.expr
import ham.expr._
import ham.expr.Type

case class Env(ctx: Id => Attempt[Type], stack: List[Type]) {
  def apply(k: Id): Attempt[Type] = ctx(k)
  def push(tpe: Type): Env        = Env(ctx, tpe :: stack)

}

object Typer {
  type E = Env
  class Err(msg: String) extends ham.errors.Err(msg)
  def parseError(msg: String) = new Err(s"Parse error: $msg")
  def typeError(msg: String)  = new Err(s"Type error: $msg")
  def error(msg: String): Err = new Err(msg)

  def typeOf(expr: Expr, ctx: Id => Attempt[Type]): Attempt[Type] =
    try {
      Right(analyse(expr, Env(ctx, Nil), Set.empty))
    } catch {
      case e: Err => ham.errors.failure(s"Failed to type check $expr\nCause: ${e.msg}", cause = e)
    }

  def analyse(node: Expr, env: E, nongen: Set[Type.Var]): Type = node match {
    case Literal(_, tpe) => tpe
    case BuiltIn(_, tpe) => tpe
    case Var(pos)        => env.stack(pos) // todo: handle error
    case Symbol(id)      => getType(id, env, nongen)
    case App(fun, arg) =>
      val funType    = analyse(fun, env, nongen)
      val argType    = analyse(arg, env, nongen)
      val resultType = new expr.Type.Var
      unify(Type.function(argType, resultType), funType)
      resultType
    case Fun(Nil, body) =>
      analyse(body, env, nongen)
    case Fun(first :: rest, body) =>
      val firstArgType = new Type.Var
      val curried      = Fun(rest, body)
      val nextEnv      = env.push(firstArgType)
      val resultType   = analyse(curried, nextEnv, nongen + firstArgType)
      Type.function(firstArgType, resultType)
  }

  def unify(t1: Type, t2: Type) {
    val type1 = prune(t1)
    val type2 = prune(t2)
    (type1, type2) match {
      case (a: Type.Var, b) =>
        if(a != b) {
          if(occursintype(a, b))
            typeError("recursive unification")
          a.instance = Some(b)
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

  def getType(name: Id, env: E, nongen: Set[Type.Var]): Type = {
    env(name) match {
      case Right(tpe) => fresh(tpe, nongen)
      case Left(msg)  => throw msg
    }
  }

  // Note: must be called with v 'pre-pruned'
  def isgeneric(v: Type.Var, nongen: Set[Type.Var]) = !(occursin(v, nongen))

  def fresh(t: Type, nongen: Set[Type.Var]) = {
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
  def prune(t: Type): Type = t match {
    case v: Type.Var if v.instance.isDefined => {
      val inst = prune(v.instance.get)
      v.instance = Some(inst)
      inst
    }
    case _ => t
  }

  // Note: must be called with v 'pre-pruned'
  def occursintype(v: Type.Var, type2: Type): Boolean = {
    prune(type2) match {
      case `v`                   => true
      case Type.Oper(name, args) => occursin(v, args)
      case _                     => false
    }
  }

  def occursin(t: Type.Var, list: Iterable[Type]) =
    list exists (t2 => occursintype(t, t2))

}
