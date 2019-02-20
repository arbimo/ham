package ham.script

import cats.implicits._
import ham.errors.{Attempt, Fail, ParseError, Succ}
import ham.expr.ModuleID
import ham.interpreter.Interpreter
import ham.lang.TypedModule
import ham.platform.Platform
import ham.prelude.Prelude

object Main extends App {

  lazy val loader = Prelude.getLoader()
  val parser      = ham.parsing.modules.Parser.default

  val res: Attempt[() => Unit] = args match {
    case Array(module)        => run(module).map(a => () => println(a))
    case Array("run", module) => run(module).map(a => () => println(a))
    case Array("type", module) =>
      typeCheck(module)
        .map(typed =>
          () => {
            typed.types.toSeq.sortBy(_._1.local).foreach { case (k, v) => println(s"$k : $v") }
        })
    case _ =>
      ham.errors.failure(s"missing module name")
  }
  res match {
    case Succ(f) => f.apply()
    case Fail(err) =>
      System.err.println(err)
      System.exit(1)
  }

  def typeCheck(f: String): Attempt[TypedModule] = {

    for {
      content <- Platform.fileSystem.readModuleSource(ModuleID(f))
      typed <- loader.loadFromSource(ModuleID(f),
                                     content,
                                     parser.declarations(_).leftMap(ParseError).toAttempt)
    } yield typed
  }

  def run(module: String): Attempt[Any] = {
    typeCheck(module).flatMap { typed =>
      typed.mod.mainFunction match {
        case None =>
          ham.errors.failure("No main")
        case Some(mainId) =>
          loader
            .definitionOf(mainId)
            .flatMap(mainExpr => Interpreter.eval(mainExpr, loader.definitionOf))
      }
    }
  }

}
