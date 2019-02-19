package hydra.leastsquares
import cats.kernel.Monoid
import fastparse.Parsed.{Failure, Success}
import ham.errors.{Attempt, ParseError}
import ham.expr.Type
import ham.parsing.AST
import ham.parsing.expr.LangParser

import scala.util.Try

object LSParser {

  import fastparse._
  import JavaWhitespace._
  val base = LangParser.default
  import base._

  def makeType(name: String): Type = Type.primitive(name)

  def num[_: P]: P[Double] = lit.map(s => Try(s.toDouble)).filter(_.isSuccess).map(_.get)

  def constantParser[_: P]: P[Constant[AST]] =
    P("constant" ~/ ident ~/ ":" ~ ident ~ "=" ~ expr ~ ";").map {
      case (name, tpe, value) => Constant(name, makeType(tpe), value)
    }
  def variableParser[_: P]: P[Variable] = P("var" ~/ ident ~ ":" ~ ident ~ ";").map {
    case (id, tpe) => Variable(id, makeType(tpe))
  }

  def constraintsParser[_: P]: P[Constraints[AST]] =
    P("subject_to" ~/ "{" ~ (expr ~ ";").rep ~ "}").map(l => Constraints(l.toList))

  def expectationsParser[_: P]: P[Map[String, Double]] =
    P("expected" ~/ "{" ~ (ident ~ "=" ~ num ~ ";").rep ~ "}").map(l => l.toMap)

  def parseAll[_: P]: P[Seq[LSModel[AST]]] =
    Pass ~ P(
      constantParser.map(c => LSModel.empty[AST].copy(constants = c :: Nil)) |
        variableParser.map(f => LSModel.empty[AST].copy(variables = f :: Nil)) |
        constraintsParser.map(cs => LSModel.empty[AST].copy(constraints = cs.l)) |
        expectationsParser.map(ex => LSModel.empty[AST].copy(expected = ex))
    ).rep ~ End

  def parse(str: String): Attempt[LSModel[AST]] = {
    fastparse.parse(str, parseAll(_)) match {
      case Success(simpleModels, _) =>
        val res: LSModel[AST] = Monoid[LSModel[AST]].combineAll(simpleModels)
        ham.errors.success(res)

      case fail: Failure =>
        ham.errors.Fail(ParseError(fail))
    }
  }
}
