package hydra.leastsquares

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
case class Variable(name: String, tpe: Type)
case class Constraints[E](l: List[E])

case class LSModel[E](
    constants: List[Constant[E]],
    variables: List[Variable],
    constraints: List[E],
    /** Partial mapping of variables to expected values used for testing */
    expected: Map[String, Double]
) {

  val moduleID = ModuleID("ls")

  def symbols: Map[String, ham.expr.Id] =
    constants.map(c => (c.name, moduleID / c.name)).toMap ++
      variables.map(f => (f.name, moduleID / f.name))
  def types: Map[ham.expr.Id, Type] =
    constants.map(c => (moduleID / c.name, c.tpe)).toMap ++
      variables.map(f => (moduleID / f.name, f.tpe))

  def mapErr[B](f: E => Attempt[B]): Attempt[LSModel[B]] = {
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
    (constants.map(c => c.name  -> c.tpe) ++
      variables.map(f => f.name -> f.tpe) //++ controls.map(c => (c.name -> c.tpe))
    ).toMap
  }

  def asState: Attempt[State] = {
    val fields = variables.map {
      case Variable(name, tpe) if tpe == Prelude.Real => ham.errors.success(StateField.real(name))
      case Variable(name, tpe)                        => ham.errors.failure(s"Field $name has unsupported type $tpe")
    }
    fields.sequence.map(fs => new State(fs.toArray))
  }
}

object LSModel {

  def empty[A]: LSModel[A] = LSModel[A](Nil, Nil, Nil, Map.empty)

  implicit def monoid[A]: Monoid[LSModel[A]] = new Monoid[LSModel[A]] {
    override def empty: LSModel[A] = LSModel.empty

    override def combine(x: LSModel[A], y: LSModel[A]): LSModel[A] =
      LSModel(
        x.constants ++ y.constants,
        x.variables ++ y.variables,
        x.constraints ++ y.constraints,
        x.expected ++ y.expected
      )
  }

  implicit val functor: Functor[LSModel] = new Functor[LSModel] {
    override def map[A, B](fa: LSModel[A])(f: A => B): LSModel[B] =
      LSModel(
        fa.constants.map(c => Constant(c.name, c.tpe, f(c.value))),
        fa.variables,
        fa.constraints.map(f),
        fa.expected
      )
  }
}
