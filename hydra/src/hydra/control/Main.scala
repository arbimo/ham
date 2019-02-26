package hydra.control

import ham.errors._
import ham.expr.{Expr, Id}
import ham.platform.Platform
import ham.prelude.Prelude

object Main extends App {

  val platform = Platform.fileSystem

  val prelude = Prelude.typedPrelude

  args match {
    case Array(filename) =>
      val res = for {
        source <- platform.readPath(filename)
        model  <- Parser.parse(source)
        syms = prelude.mod.unqualifiedNames ++ model.symbols
        modExpr <- model.mapErr(x => Expr.fromAST(x, id => syms.get(id)))
        teb     <- Controller.toTeb(modExpr)
        sol     <- teb.solve(0.1)
      } yield {
        sol
      }

      res match {
        case Succ(sol) => println(sol.format())
        case Fail(err) =>
          println(err)
          sys.exit(1)
      }

    case x =>
      System.err.println(s"Expected a single filename argument but got: ${x.mkString(" ")}")
      sys.exit(1)
  }

}
