package ham.platform

import java.nio.file.{Files, Path, Paths}

import ham.errors.Attempt
import ham.expr.ModuleID

trait Platform {

  def readModuleSource(m: ModuleID): Attempt[String]

}

object Platform {

  // must be a def and not a val so that Path.get is not evaluated at static initialization.
  // this occurs when compiling ahead of time with GraalVM
  def default = new Default(Paths.get("").toAbsolutePath)


  class Default(wd: Path) extends Platform {
    override def readModuleSource(m: ModuleID): Attempt[String] = {
      val f = Paths.get(wd.toAbsolutePath.toString, m.name + ".ham")
      try {
        val lines = Files.readAllLines(f)
        ham.errors.success(lines.toArray.mkString("\n"))
      } catch {
        case e: Exception => ham.errors.failure(s"Cannot read file $f", cause = e)
      }

    }
  }


}
