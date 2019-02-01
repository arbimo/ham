package ham.integration

import java.nio.file.{Files, Path, Paths}

import ham.expr.ModuleID
import ham.interpreter.Interpreter
import ham.parsing.ParseTest
import ham.prelude.Prelude

object Tests extends App {

  println("\n======= Valid tests =======")
  forEachHamFilesIn("/tests/positive", checkValid)

  println("\n======= Invalid tests =======")
  forEachHamFilesIn("/tests/negative", checkInvalid)


  def forEachHamFilesIn(dir: String, run: Path => Unit): Unit = {
    val clazz = this.getClass
    val res = clazz.getResource(dir)
    val uri = res    .toURI
    val path = Paths.get(uri)
    Files.list(path)
      .forEach(p => run(p))
  }


  def checkValid(p: Path): Unit = {
    println
    val id = p.getFileName.toString.replaceAll(".ham", "")
    val content = Files.readAllLines(p).toArray().mkString("\n")
    val loader = Prelude.getLoader()
    loader.loadFromSource(ModuleID(id), content) match {
      case Right(typed) =>
        println(s"Successfully typed $id")
        typed.types.foreach { case (k, v) => println(s"$k : $v") }

        typed.mod.mainFunction match {
          case None =>
            println("No main")
          // no main to try running
          case Some(mainId) =>
            loader.definitionOf(mainId) match {
              case Right(mainExpr) =>
                Interpreter.eval(mainExpr, loader.definitionOf) match {
                  case Right(res) => println(s"main = $res")
                  case Left(err) =>
                    System.err.println(s"Error while evaluating main: ${err.msg}")
                    err.printStackTrace()
                }
              case Left(err) =>
                System.out.println("Could not find definition of main method")
                System.exit(1)
            }


        }
      case Left(err) =>
        System.err.println(s"Failed to type check: $p")
        System.err.println(content)
        System.err.println(err)
        err.printStackTrace()
        System.exit(1)
    }
  }

  def checkInvalid(p: Path): Unit = {
    println
    val id = p.getFileName.toString.replaceAll(".ham", "")
    val content = Files.readAllLines(p).toArray().mkString("\n")
    val loader = Prelude.getLoader()
    loader.loadFromSource(ModuleID(id), content) match {
      case Right(typed) =>
        System.err.println(s"Error: successfully typed $p")
        System.err.println(content)
        typed.types.foreach { case (k, v) => System.err.println(s"$k : $v") }
        System.exit(1)
      case Left(err) =>
        println(s"OK: failed to type check: $id")
        println(err)
    }
  }

}
