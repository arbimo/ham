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
    platform
      .readPath("content.txt")
      .assertSucceedsTo("AA AA\nBB BB\n\nDD DD")

    platform
      .readPath("tests/content.txt")
      .assertSucceedsTo("AA")

    platform.readPath("does-not-exists.txt").assertFails
  }

}
