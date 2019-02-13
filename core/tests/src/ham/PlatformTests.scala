package ham

import ham.platform.Platform
import ham.errors._
import minitest._

object PlatformTests extends SimpleTestSuite {

  test("load resource file (from resources)") {
    testsResourceRead(Platform.resources)
  }
  test("laod resource file (from file system or else resources)") {
    testsResourceRead(Platform.fileSystemOrElseResources)
  }


  def testsResourceRead(platform: Platform): Unit = {
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
