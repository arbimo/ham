package ham.typing

//import fastparse.Parsed.{Failure, Success}
//import ham.Ham
//import ham.model._
//import TypeSystem._
//
//object TypeChecker {
//
//  val Real = Oper("Real", Seq())
//  val Bool = Oper("Bool", Seq())
//
//  def fun(tpe: Type, tpe2: Type): Type = TypeSystem.Function(tpe, tpe2)
//  def fun(tpe1: Type, tpe2: Type, tpe3: Type): Type = fun(tpe1, fun(tpe2, tpe3))
//  def fun(tpe1: Type, tpe2: Type, tpe3: Type, tpe4: Type): Type = fun(tpe1, fun(tpe2, fun(tpe3, tpe4)))
//
//  val env: TypeSystem.Env = Map(
//    "PI" -> Real,
//    "+" -> fun(Real, Real, Real),
//    "*" -> fun(Real, Real, Real),
//    "/" -> fun(Real, Real, Real),
//
//    "<" -> fun(Real, Real, Bool),
//    "<=" -> fun(Real, Real, Bool),
//    ">" -> fun(Real, Real, Bool),
//    ">=" -> fun(Real, Real, Bool),
//    "ite" -> {
//      val var3 = TypeSystem.newVariable
//      fun(Bool, var3, var3, var3)
//    },
//    "cos" -> fun(Real, Real),
//    "sin" -> fun(Real, Real),
//    "tan" -> fun(Real, Real),
//    "abs" -> fun(Real, Real)
//  )
//
//  def typeOf(e: AST, defs: Map[String, Type]): Type = TypeSystem.prune(TypeSystem.analyse(Interop.expr2syntax(e), env ++ defs))
//}
//
//object Interop extends App {
//
//  def expr2syntax(e: AST): SyntaxNode = e match {
//    case Num(i) => Ident(i.toString())
//    case Sym(name) => Ident(name)
//    case Application(f, arg) =>
//      Apply(expr2syntax(f), expr2syntax(arg))
//    case ham.model.Lambda(a, body) => ham.typing.Lambda(a.name, expr2syntax(body))
//  }
//
//  val prog = List(
//    "function fst(a, b) { a }",
//    "x = 1 + 2",
//    "y = x",
//    "function incx(a) { a + x }",
//    "function add(a, b) { a + b }",
//    "inc = add(1)",
//    "function min(a, b) { ite(a < b, a, b) }",
//  )
//
//  val env: TypeSystem.Env = Map(
//    "+" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer)),
//    "+" -> {
//      val var1 = TypeSystem.newVariable
//      TypeSystem.Function(var1, TypeSystem.Function(var1, var1))
//    },
//    "<" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Bool)),
//    "ite" -> {
//      val var3 = TypeSystem.newVariable
//      TypeSystem.Function(TypeSystem.Bool, TypeSystem.Function(var3, TypeSystem.Function(var3, var3)))
//    },
//  )
//
//  prog.foldLeft(env) { case (env, expr) =>
//    fastparse.parse(expr, Ham.declaration(_)) match {
//      case Success(Decl(id, ast), index) =>
//        val syntax = expr2syntax(ast)
//        val tpe = TypeSystem.analyse(syntax, env)
//        println(expr)
//        println(syntax)
//        println(TypeSystem.string(tpe))
//        println()
//        env.updated(id.name, tpe)
//      case fail @ Failure(label, index, extra) =>
//        println(fail)
//        ???
//    }
//
//  }
//
//}
