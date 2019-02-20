package hydra

import cats.implicits._
import ham.errors.{Attempt, ParseError}
import ham.expr.{Expr, ModuleID, Type}
import ham.lang.Env
import ham.prelude.Prelude
import ham.typing.Typer
import hydra.reductions.Reductions
import org.scalacheck.{Arbitrary, Gen}

/** A contextualized expression that is bundled with its
  * environment that should define everything needed for its evaluation */
case class CExpr(e: Expr, ctx: Env) {
  def tpe: Attempt[Type] = Typer.typeOf(e, ctx.typeOf)
  def reduced: CExpr =
    CExpr(Reductions.expand(e, Nil, ctx.definitionOf(_).toOption), ctx)
}

object CExpr {

  private def parseExpr(str: String): Attempt[CExpr] = {
    val env = Prelude.getLoader()
    env
      .loadFromSource(
        ModuleID("test"),
        s"main = $str",
        ham.parsing.modules.Parser.default.declarations(_).leftMap(ParseError).toAttempt)
      .flatMap { tm =>
        tm.mod.definition(tm.mod.symbolNameToId("main"))
      }
      .map(expr => CExpr(expr, env))

  }

  // must be lazy because tests are not run in case of an ExceptionInInitializer
  lazy val exprs: List[CExpr] = List(
    "1 + 2",
    "1 * 2 + 4",
    "true || false",
    "false && true",
    "1 < 2 || true",
    "max",
    "sin"
  ).map(parseExpr).sequence.unsafeGet

  implicit def arbitraryExpr: Arbitrary[CExpr] = Arbitrary(Gen.oneOf(exprs))

}
