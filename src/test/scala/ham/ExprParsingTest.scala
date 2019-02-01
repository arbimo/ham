package ham

import fastparse.Parsed.{Failure, Success}
import minitest._
import fastparse._
import ham.parsing.AST
import ham.parsing.expr.LangParser

object ExprParsingTest extends SimpleTestSuite {
  import fastparse.SingleLineWhitespace._
  def parser[_: P]: P[AST] = Pass ~ LangParser.default.expr ~ End

  def parses(str: String): Unit = {
    fastparse.parse(str, parser(_)) match {
      case Success(value, index) => println("OK: " + value)
      case fail @ Failure(label, index, extra) =>
        println("KO: " + str)
        println(fail)
        assert(false)
    }
  }
  test("should parse") {
    parses("plus(a, b)")
    parses("d + a")
    parses("plus(a + d, x)")
    parses("a + plus(b, c)")
    parses("1 + a")
    parses("f(a)")
    parses("f(f(a))")
    parses("a + f(a)")
    parses("f(a) + a")
    parses("a + b * c")
    parses("a * b + c")
    parses("dot(x) == v *  cos(theta)")
    assertEquals(2, 1 + 1)
  }

}