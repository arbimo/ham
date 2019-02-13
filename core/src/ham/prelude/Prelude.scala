package ham.prelude

import ham.expr
import ham.expr.{BuiltIn, Type}
import ham.lang.{Import, Module, SimpleLoader, TypedModule}

object Prelude {
  import Type._

  val Bool: Type = Type.primitive("Bool")
  val Real: Type = Type.primitive("Real")


  val prelude = new Module("prelude", Nil, Map(
    "ite" -> BuiltIn("bool.ite", forall(t => function(Bool, t, t, t))),
    "==" -> BuiltIn("bool.eq", forall(t => function(t, t, Bool))),

    "true" -> BuiltIn("bool.true", Bool),
    "false" -> BuiltIn("bool.false", Bool),
    "||" -> BuiltIn("bool.or", function(Bool, Bool, Bool)),
    "&&" -> BuiltIn("bool.and", function(Bool, Bool, Bool)),
    "not" -> BuiltIn("bool.not", function(Bool, Bool)),

    "<" -> BuiltIn("real.lt", Type.function(Real, Real, Bool)),
    "<=" -> BuiltIn("real.leq", Type.function(Real, Real, Bool)),

    "+" -> BuiltIn("real.add", function(Real, Real, Real)),
    "-" -> BuiltIn("real.sub", function(Real, Real, Real)),
    "/" -> BuiltIn("real.div", function(Real, Real, Real)),
    "*" -> BuiltIn("real.mul", function(Real, Real, Real)),

    "PI" -> BuiltIn("real.PI", Real),
    "cos" -> BuiltIn("real.cos",function(Real, Real)),
    "sin" -> BuiltIn("real.sin", function(Real, Real)),
  ))

  val typedPrelude: TypedModule = prelude.typeCheck(x => ham.errors.failure(s"Undefined id $x")) match {
    case Right(types) => TypedModule(prelude, types)
    case Left(err) =>
      throw new RuntimeException("FATAL: Prelude failed to type check", err)
  }

  def getLoader(): SimpleLoader =
    new SimpleLoader(predef = List(prelude), defaultImports = List(Import.UnQualified(prelude.id)))


}
