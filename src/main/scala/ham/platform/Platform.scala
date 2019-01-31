package ham.platform

import java.nio.file.{Files, Path, Paths}

import ham.errors.Attempt
import ham.expr.ModuleID

trait Platform {

  def readModuleSource(m: ModuleID): Attempt[String]

}

object Platform {


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
