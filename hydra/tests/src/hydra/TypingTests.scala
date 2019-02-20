package hydra
import minitest.SimpleTestSuite
import minitest.laws.Checkers

object TypingTests extends SimpleTestSuite with Checkers {

  test("expr instances are well typed") {
    check1((c: CExpr) => { c.tpe.assertSucceeds; true })
  }

}
