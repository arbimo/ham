package hydra.reductions

import ham.errors._
import ham.expr.Type
import hydra.CExpr
import minitest.SimpleTestSuite
import minitest.laws.Checkers

object ReductionTests extends SimpleTestSuite with Checkers {

  def expansionMaintainsType(ce: CExpr): Boolean =
    ce.tpe match {
      case Succ(tpe) =>
        val reduced = ce.reduced
        reduced.tpe match {
          case Succ(typeAfter) =>
            Type.equivalent(tpe, typeAfter)
          case x => x.assertSucceeds
        }
        true
      case _ => false
    }

  def expansionIsIdempotent(ce: CExpr): Boolean = {
    val ex1 = ce.reduced
    val ex2 = ce.reduced
    ex1 == ex2
  }

  test("expansion maintains type") {
    check1(expansionMaintainsType)
  }

  test("expansion is idempotent") {
    check1(expansionIsIdempotent)
  }

}
