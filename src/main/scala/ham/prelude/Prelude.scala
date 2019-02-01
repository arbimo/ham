package ham.prelude

import ham.expr
import ham.expr.{BuiltIn, Type}
import ham.lang.{Import, Module, SimpleLoader}

object Prelude {
  import Type._

  val Bool = Type.primitive("Bool")
  val Real = Type.primitive("Real")


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

    "cos" -> BuiltIn("real.cos",function(Real, Real)),
    "sin" -> BuiltIn("real.sin", function(Real, Real)),

  ))

  def getLoader(): SimpleLoader =
    new SimpleLoader(predef = List(prelude), defaultImports = List(Import.UnQualified(prelude.id)))


}
