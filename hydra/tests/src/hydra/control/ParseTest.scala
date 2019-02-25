package hydra.control

import cats.implicits._
import ham.expr._
import ham.platform.Platform
import ham.prelude.Prelude
import hydra.TestsMain
import hydra.reductions.Reductions
import minitest._

import scala.annotation.tailrec

object ParseTest extends SimpleTestSuite with TestsMain {

  val platform = Platform.resources

  val prelude = Prelude.typedPrelude

  test("parse car-1D") {

    val res = for {
      source <- platform.readPath("control/1d-car.hy")
      model  <- Parser.parse(source)
      syms = prelude.mod.unqualifiedNames ++ model.symbols
      modExpr <- model.mapErr(x => Expr.fromAST(x, id => syms.get(id)))
    } yield {

      Controller.toTeb(modExpr)

      val csts                     = modExpr.constants.map(c => modExpr.moduleID / c.name -> c.value).toMap
      val defs: Id => Option[Expr] = id => csts.get(id).orElse(prelude.mod.definition(id).toOption)
      val constraints              = modExpr.globalConstraints ++ modExpr.initConstraints ++ modExpr.finalConstraints

      model
    }

    res.assertSucceeds

  }

}
