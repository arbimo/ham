package hydra

import ham.platform.Platform
import ham.errors._
import minitest._

object ParsingTests extends SimpleTestSuite with TestsMain {

  val platform = Platform.resources

  test("simple parsing") {
    val x = for {
      source <- platform.readPath("car.hy")
      mod <- Parser.parse(source)
    } yield mod

    assertSucceeds(x)
  }




}
