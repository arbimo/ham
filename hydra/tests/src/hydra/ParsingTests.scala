package hydra

import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors

import cats.implicits._
import ham.platform.Platform
import ham.errors._
import hydra.leastsquares.{LS, LSParser}
import minitest._

object ParsingTests extends SimpleTestSuite with TestsMain {

  val platform = Platform.resources

  def forEachFileIn(dir: String, run: Path => Unit): Int = {
    val res = Thread.currentThread().getContextClassLoader.getResource(dir)
//    val res   = clazz.getResource(dir)
    val uri   = res.toURI
    val path  = Paths.get(uri)
    val files = Files.list(path)
    var cnt   = 0
    files.forEach(p => { cnt += 1; run(p) })
    cnt
  }

  def runAndValidate(p: Path): Unit = {
    val pb     = p.getFileName.toString
    val source = Files.readAllLines(p).toArray().mkString("\n") //platform.readPath(p.toString)
    val x = for {
      mod <- LS.parse(source)
      sol <- LS.solve(mod)
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

  test("least-square linear tests") {
    val numRuns = forEachFileIn("least-squares/linear", runAndValidate)
    assert(numRuns >= 2, "Was expecting at least 2 test files")
  }

}
