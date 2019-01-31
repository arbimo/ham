package ham.expr

import ham.errors.Attempt

import scala.reflect.ClassTag

case class ModuleID(name: String) {
  override def toString: String = name
  def /(localId: String): Id = Id(this, localId)
}
case class Id(module: ModuleID,  name: String) {
  def local: String = name
  def global: String = s"$module.$name"

  override def toString: String = global
}

abstract class Expr

final case class App(fun: Expr, arg: Expr) extends Expr
final case class Fun(params: List[String], body: Expr) extends Expr
final case class Var(pos: Int) extends Expr
final case class Symbol(id: Id) extends Expr
final case class BuiltIn(name: String, tpe: Type) extends Expr
final case class Literal(value: Any, tpe: Type) extends Expr



object Expr {
  import ham.hybrid._

  object types {
    val Real = Type.primitive("Real")
  }


  import ham.parsing._


  def fromAST(ast: AST, lookup: String => Option[Id]): Attempt[Expr] =
    try {
      Right(unsafeFromAST(ast, lookup, Nil))
    } catch {
      case e: ham.errors.Err => Left(new ham.errors.Err(s"error while processing AST: $ast\n ${e.getMessage}", cause = e))
      case e: Throwable => Left(new ham.errors.Err(s"error while processing AST: $ast\n ${e.getMessage}", cause = e))
    }

  @throws[Error]
  private def unsafeFromAST(ast: AST, lookup: String => Option[Id], stack: List[Sym]): Expr = ast match {
    case Num(n) if n.isExactDouble =>
      Literal(n.toDouble, types.Real) // TODO: need to abstract over literals
    case Num(n) =>
      throw ham.errors.error(s"Number $n cannot be exactly converted to Double")
    case sym @ Sym(_) if stack.contains(sym) =>
      Var(stack.indexOf(sym))
    case Sym(name) =>
      lookup(name) match {
        case Some(id) => Symbol(id)
        case None => throw ham.errors.error(s"Unknown symbol $name")
      }
    case Application(f, a) =>
      App(unsafeFromAST(f, lookup, stack), unsafeFromAST(a, lookup, stack))

    case Lambda(arg, body) =>
      unsafeFromAST(body, lookup, arg :: stack) match {
        case Fun(params, body) => Fun(arg.name :: params, body)
        case expr => Fun(arg.name :: Nil, expr)
      }

  }

  def symbolOccurences(e: Expr): Set[Id] = e match {
    case App(a, b) => symbolOccurences(a) ++ symbolOccurences(b)
    case Fun(_, body) => symbolOccurences(body)
    case Symbol(id) => Set(id)
    case _ => Set.empty
  }
}