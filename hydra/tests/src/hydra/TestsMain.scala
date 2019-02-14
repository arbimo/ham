package hydra

import minitest._
import minitest.api.AbstractTestSuite

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

/** A mixin trait that provides a main entry point to minitest suites. */
trait TestsMain { self: AbstractTestSuite =>

  def main(args: Array[String]): Unit = {
    implicit val ctx = scala.concurrent.ExecutionContext.global

    properties.foreach { f =>
      val runner = f.apply(())
      runner.onComplete {
        case Success(x) =>
          println(x.formatted(f.name, withColors = true))
        case Failure(exception) =>
          println("Unexpected failure when running test")
          exception.printStackTrace()
          sys.exit()
      }
      Await.result(runner, Duration.Inf)
    }
  }

}
