package ham.parsing

import ham.expr.{BuiltIn, ModuleID, Type}
import ham.lang._
import minitest._

object ParseTest extends SimpleTestSuite {

  val Bool = Type.primitive("Bool")
  val Real = Type.primitive("Real")

  val ITE = {
    val t = new Type.Var
    BuiltIn("ite", Type.function(Bool, t, t, t))
  }

  val prelude = new Module("prelude", Nil, Map(
    "ite" -> ITE,
    "<" -> BuiltIn("<", Type.function(Real, Real, Bool))
  ))

  def getLoader(): SimpleLoader = new SimpleLoader(predef = List(prelude), defaultImports = List(Import.UnQualified(prelude.id)))

  def parses(content: String): Unit = {
    val loader = getLoader()
    val res = for {
      - <- loader.loadFromSource(ModuleID("test"), content)
    } yield ()
    res match {
      case Right(_) =>
      case Left(err) => throw err
    }
//    match {
//      case Valid(a) =>
//      case Invalid(e) =>
//        e.toList.foreach(println)
//        throw new AssertionError()
//    }
  }

  test("base") {
    parses("max(x, y) = x < y")
    parses("max(x, y) = ite(x < y, y, x)")
  }

}
