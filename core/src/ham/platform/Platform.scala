package ham.platform

import java.nio.file.{Files, Path, Paths}

import ham.errors.Attempt
import ham.expr.ModuleID

trait Platform {

  def readModuleSource(m: ModuleID, extension: String = "ham"): Attempt[String] = {
    if(extension.isEmpty)
      readPath(m.name)
    else
      readPath(m.name + "." + extension)
  }

  def readPath(name: String): Attempt[String]

}

object Platform {

  // must be a def and not a val so that Path.get is not evaluated at static initialization.
  // this occurs when compiling ahead of time with GraalVM
  def fileSystem: Platform                = new Default(Paths.get("").toAbsolutePath)
  def resources: Platform                 = Resources
  def fileSystemOrElseResources: Platform = new PlatformWithFallback(fileSystem, resources)

  class Default(wd: Path) extends Platform {

    override def readPath(name: String): Attempt[String] = {
      val f = Paths.get(wd.toAbsolutePath.toString, name)
      try {
        val lines = Files.readAllLines(f)
        ham.errors.success(lines.toArray.mkString("\n"))
      } catch {
        case e: Exception => ham.errors.failure(s"Cannot read file $f", cause = e)
      }
    }
  }

  object Resources extends Platform {
    override def readPath(name: String): Attempt[String] =
      try {
//      {
//      val tmp = Thread.currentThread().getContextClassLoader.getResource(name)
//      println(tmp)
//      val path = Paths.get(tmp.toURI)
//
//      val clazz = this.getClass
//      val res = clazz.getResource("tests")
//      val uri = res.toURI
////      val path = Paths.get(uri)
//      Files.list(path)
//        .forEach(p => println)
//    }

//      val clazz = this.getClass
//      val res = clazz.getResource(name)
        val res = Thread.currentThread().getContextClassLoader.getResources(name)
        if(res.hasMoreElements) {
          val url = res.nextElement()
          if(res.hasMoreElements) {
            ham.errors.failure(s"More than one resource named $name")
          } else {
            val path  = Paths.get(url.toURI)
            val lines = Files.readAllLines(path)
            ham.errors.success(lines.toArray.mkString("\n"))
          }
        } else {
          ham.errors.failure(s"No resource named $name")
        }

      } catch {
        case e: Exception => ham.errors.failure(s"Cannot read resource file $name", cause = e)
      }
  }

  class PlatformWithFallback(p1: Platform, p2: Platform) extends Platform {
    override def readPath(name: String): Attempt[String] = {
      p1.readPath(name) match {
        case Left(err) =>
          p2.readPath(name) match {
            case Left(err2) =>
              Left(ham.errors.combined(err, err2))
            case x => x
          }
        case x => x
      }
    }
  }

}
