package hydra

import java.nio.file.{Files, Path, Paths}

import cats.implicits._
import ham.platform.Platform
import hydra.leastsquares.LS
import minitest._

object LeastSquareTests extends SimpleTestSuite {

  val platform = Platform.resources

  def forEachFileIn(dir: String, run: Path => Unit): Int = {
    val res   = Thread.currentThread().getContextClassLoader.getResource(dir)
    val uri   = res.toURI
    val path  = Paths.get(uri)
    val files = Files.list(path)
    var cnt   = 0
    files.forEach(p => { cnt += 1; run(p) })
    cnt
  }

  def runAndValidate(p: Path, linear: Boolean): Unit = {
    val pb     = p.getFileName.toString
    val source = Files.readAllLines(p).toArray().mkString("\n") //platform.readPath(p.toString)
    val x = for {
      mod <- LS.parse(source)
      sol <- if(linear) LS.solveLinear(mod) else LS.solveNonlinear(mod)
    } yield {
      mod.expected.foreach {
        case (variable, expectedValue) =>
          if(math.abs(sol(variable) - expectedValue) > 10E-5) {
            throw new AssertionError(s"""In problem $pb, for variable $variable
                                        |Expected: $expectedValue
                                        |Result:   ${sol(variable)}""".stripMargin)
          }
      }
      sol
    }
    x.assertSucceeds
  }

  val numLinearInstances    = 2
  val numNonLinearInstances = 1

  test("linear least-squares") {
    val numRuns = forEachFileIn("least-squares/linear", runAndValidate(_, linear = true))
    assert(numRuns >= numLinearInstances, s"Was expecting at least $numLinearInstances test files")
  }

  test("nonlinear least-squares (linear instances)") {
    val numRuns = forEachFileIn("least-squares/linear", runAndValidate(_, linear = false))
    assert(numRuns >= numLinearInstances, s"Was expecting at least $numLinearInstances test files")
  }

  test("nonlinear least-squares (nonlinear instances)") {
    val numRuns = forEachFileIn("least-squares/nonlinear", runAndValidate(_, linear = false))
    assert(numRuns >= numNonLinearInstances,
           s"Was expecting at least $numNonLinearInstances test files")
  }

}
