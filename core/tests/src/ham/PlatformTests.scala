package ham

import ham.platform.Platform
import ham.errors._
import minitest._

object PlatformTests extends SimpleTestSuite {

  test("load resource file") {
    val platform = Platform.resources

    assertSucceedsTo(
      platform.readPath("content.txt"),
      "AA AA\nBB BB\n\nDD DD")

    assertSucceedsTo(
      platform.readPath("tests/content.txt"),
      "AA"
    )

    assertFails(platform.readPath("does-not-exists.txt"))

  }

}
