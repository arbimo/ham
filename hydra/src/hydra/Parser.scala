package hydra

//import cats._
import cats.{Applicative, Functor}
import cats.implicits._
import cats.kernel.Monoid
import fastparse.Parsed.{Failure, Success}
import ham.errors.{Attempt, Err, Fail, ParseError, Succ}
import ham.expr.{Expr, Id, ModuleID, Type}
import ham.parsing.AST
import ham.parsing.expr.LangParser
import ham.prelude.Prelude
import ham.state.{State, StateField}
import spire.math.{Jet, JetDim}

case class Constant[E](name: String, tpe: Type, value: E)
case class Fluent(name: String, tpe: Type)
//case class Control(name: String, tpe: Type)
//case class Dynamics[E](l: List[Dynamic[E]])
//case class Dynamic[E](fluent: String, value: E)
case class Constraints[E](l: List[E])
//
case class HamModel[E](
    constants: List[Constant[E]],
    fluents: List[Fluent],
//              controls: List[Control],
//              dynamics: List[Dynamic[E]],
    constraints: List[E]
) {

  val moduleID = ModuleID("ham")

  def symbols: Map[String, ham.expr.Id] =
    constants.map(c => (c.name, moduleID / c.name)).toMap ++
      fluents.map(f => (f.name, moduleID / f.name))
  def types: Map[ham.expr.Id, Type] =
    constants.map(c => (moduleID / c.name, c.tpe)).toMap ++
      fluents.map(f => (moduleID / f.name, f.tpe))

  def mapErr[B](f: E => Attempt[B]): Attempt[HamModel[B]] = {
    val fThrow: E => B = f(_) match {
      case Succ(v) => v
      case Fail(e) => throw e
    }
    try {
      Succ(this.map(fThrow))
    } catch {
      case e: Err => Fail(e)
    }
  }

  def definitions: Map[String, Type] = {
    (constants.map(c => c.name -> c.tpe) ++
      fluents.map(f => f.name  -> f.tpe) //++ controls.map(c => (c.name -> c.tpe))
    ).toMap
  }

  def asState: Attempt[State] = {
    val fields = fluents.map {
      case Fluent(name, tpe) if tpe == Prelude.Real => ham.errors.success(StateField.real(name))
      case Fluent(name, tpe)                        => ham.errors.failure(s"Field $name has unsupported type $tpe")
    }
    fields.sequence.map(fs => new State(fs.toArray))
  }
}

object HamModel {

  def empty[A]: HamModel[A] = HamModel[A](Nil, Nil, Nil)

  implicit def monoid[A]: Monoid[HamModel[A]] = new Monoid[HamModel[A]] {
    override def empty: HamModel[A] = HamModel.empty

    override def combine(x: HamModel[A], y: HamModel[A]): HamModel[A] =
      HamModel(
        x.constants ++ y.constants,
        x.fluents ++ y.fluents,
        x.constraints ++ y.constraints
      )
  }

  implicit val functor: Functor[HamModel] = new Functor[HamModel] {
    override def map[A, B](fa: HamModel[A])(f: A => B): HamModel[B] =
      HamModel(
        fa.constants.map(c => Constant(c.name, c.tpe, f(c.value))),
        fa.fluents,
        fa.constraints.map(f)
      )
  }
}

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
//  def controlParser[_: P]: P[Control] = P("control" ~/ ident ~ ":" ~ ident ~ ";").map {
//    case (id, tpe) => Control(id.name, makeType(tpe.name))
//  }
//  def dynamicParser[_: P]: P[Dynamic[AST]] = P("dot" ~ "(" ~ ident ~ ")" ~ "=" ~ expr ~ ";").map {
//    case (id, value) => Dynamic(id.name, value)
//  }
//  def dynamicsParser[_: P] :P[Dynamics[AST]] = P("dynamics" ~ "{" ~ dynamicParser.rep ~ "}").map(l => Dynamics(l.toList))

  def constraintsParser[_: P]: P[Constraints[AST]] =
    P("subject_to" ~/ "{" ~ (expr ~ ";").rep ~ "}").map(l => Constraints(l.toList))

  def parseAll[_: P]: P[Seq[HamModel[AST]]] =
    Pass ~ P(
      constantParser.map(c => HamModel.empty[AST].copy(constants = c :: Nil)) |
        fluentParser.map(f => HamModel.empty[AST].copy(fluents = f :: Nil)) |
        constraintsParser.map(cs => HamModel.empty[AST].copy(constraints = cs.l))
    ).rep ~ End

  def parse(str: String): Attempt[HamModel[AST]] = {
    fastparse.parse(str, parseAll(_)) match {
      case Success(simpleModels, _) =>
        val res: HamModel[AST] = Monoid[HamModel[AST]].combineAll(simpleModels)
        ham.errors.success(res)

      case fail: Failure =>
        ham.errors.Fail(ParseError(fail))
    }
  }
}
