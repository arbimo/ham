package hydra.control

import cats.implicits._
import ham.expr._
import ham.platform.Platform
import ham.prelude.Prelude
import hydra.TestsMain
import hydra.reductions.Reductions
import minitest._

import scala.annotation.tailrec

object ControlSolveTests extends SimpleTestSuite {

  val platform = Platform.resources

  val prelude = Prelude.typedPrelude

  for(pb <- List("control/1d-car.hy", "control/descent.hy")) {

    test(s"solve $pb") {

      val res = for {
        source <- platform.readPath(pb)
        model  <- Parser.parse(source)
        syms = prelude.mod.unqualifiedNames ++ model.symbols
        modExpr <- model.mapErr(x => Expr.fromAST(x, id => syms.get(id)))
        teb     <- Controller.toTeb(modExpr)
        sol     <- teb.solve(0.1)
      } yield {
        sol
      }

      res.assertSucceeds

    }
  }

}
