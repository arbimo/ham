package hydra.control
import cats.kernel.Monoid
import fastparse.Parsed.{Failure, Success}
import ham.errors.{Attempt, ParseError}
import ham.expr.Type
import ham.parsing.AST
import ham.parsing.expr.LangParser

//
object Parser {

  import fastparse._
  import JavaWhitespace._
  val base = LangParser.default
  import base._

  def makeType(name: String): Type = Type.primitive(name)

  def constantParser[_: P]: P[Constant[AST]] =
    P("constant" ~/ ident ~/ ":" ~ ident ~ "=" ~ expr ~ ";").map {
      case (name, tpe, value) => Constant(name, makeType(tpe), value)
    }
  def fluentParser[_: P]: P[Fluent] = P("fluent" ~/ ident ~ ":" ~ ident ~ ";").map {
    case (id, tpe) => Fluent(id, makeType(tpe))
  }
  def controlParser[_: P]: P[Control] = P("control" ~/ ident ~ ":" ~ ident ~ ";").map {
    case (id, tpe) => Control(id, makeType(tpe))
  }
  def dynamicParser[_: P]: P[Dynamic[AST]] = P("dot" ~ "(" ~ ident ~ ")" ~ "=" ~ expr ~ ";").map {
    case (id, value) => Dynamic(id, value)
  }
  def dynamicsParser[_: P]: P[Dynamics[AST]] =
    P("dynamics" ~ "{" ~ dynamicParser.rep ~ "}").map(l => Dynamics(l.toList))

  def constraintsParser[_: P](sectionName: String): P[Constraints[AST]] =
    P(sectionName ~/ "{" ~ (expr ~ ";").rep ~ "}").map(l => Constraints(l.toList))

  def parseAll[_: P]: P[Seq[ControlModel[AST]]] =
    Pass ~ P(
      constantParser.map(c => ControlModel.empty[AST].copy(constants = c :: Nil)) |
        fluentParser.map(f => ControlModel.empty[AST].copy(fluents = f :: Nil)) |
        controlParser.map(c => ControlModel.empty[AST].copy(controls = c :: Nil)) |
        dynamicsParser.map(dyns => ControlModel.empty[AST].copy(dynamics = dyns.l)) |
        constraintsParser("subject_to").map(cs =>
          ControlModel.empty[AST].copy(globalConstraints = cs.l)) |
        constraintsParser("initially").map(cs =>
          ControlModel.empty[AST].copy(initConstraints = cs.l)) |
        constraintsParser("finally").map(cs =>
          ControlModel.empty[AST].copy(finalConstraints = cs.l))
    ).rep ~ End

  def parse(str: String): Attempt[ControlModel[AST]] = {
    fastparse.parse(str, parseAll(_)) match {
      case Success(simpleModels, _) =>
        val res: ControlModel[AST] = Monoid[ControlModel[AST]].combineAll(simpleModels)
        ham.errors.success(res)

      case fail: Failure =>
        ham.errors.Fail(ParseError(fail))
    }
  }
}
