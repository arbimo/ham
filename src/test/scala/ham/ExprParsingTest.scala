package ham

import fastparse.Parsed.{Failure, Success}
import minitest._
import fastparse._

object ExprParsingTest extends SimpleTestSuite {

  def parses(str: String): Unit = {
    fastparse.parse(str, OperatorClimbing.base.complete(_)) match {
      case Success(value, index) => println("OK: " + value)
      case fail @ Failure(label, index, extra) =>
        println("KO: " + str)
        println(fail)
        assert(false)
    }
  }
  test("should be") {
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

  test("should not be") {
    assert(1 + 1 != 3)
  }

  test("should throw") {
    class DummyException extends RuntimeException("DUMMY")
    def test(): String = throw new DummyException

    intercept[DummyException] {
      test()
    }
  }

  test("test result of") {
    assertResult("hello world") {
      "hello" + " " + "world"
    }
  }
}