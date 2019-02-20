package hydra.control

import cats.Functor
import cats.implicits._
import cats.kernel.Monoid
import ham.errors.{Attempt, Err, Fail, Succ}
import ham.expr.{ModuleID, Type}
import ham.prelude.Prelude
import ham.state.{State, StateField}

case class Constant[E](name: String, tpe: Type, value: E)
case class Fluent(name: String, tpe: Type)
case class Control(name: String, tpe: Type)
case class Dynamics[E](l: List[Dynamic[E]])
case class Dynamic[E](fluent: String, value: E)
case class Constraints[E](l: List[E])

case class ControlModel[E](
    constants: List[Constant[E]],
    fluents: List[Fluent],
    controls: List[Control],
    dynamics: List[Dynamic[E]],
    globalConstraints: List[E],
    initConstraints: List[E],
    finalConstraints: List[E]
) {

  val moduleID = ModuleID("ham")

  def symbols: Map[String, ham.expr.Id] =
    constants.map(c => c.name -> moduleID / c.name).toMap ++
      fluents.map(f => f.name  -> moduleID / f.name) ++
      controls.map(c => c.name -> moduleID / c.name)
  def types: Map[ham.expr.Id, Type] =
    constants.map(c => (moduleID / c.name, c.tpe)).toMap ++
      fluents.map(f => (moduleID / f.name, f.tpe)) ++
      controls.map(c => (moduleID / c.name, c.tpe))

  def mapErr[B](f: E => Attempt[B]): Attempt[ControlModel[B]] = {
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

object ControlModel {

  def empty[A]: ControlModel[A] = ControlModel[A](Nil, Nil, Nil, Nil, Nil, Nil, Nil)

  implicit def monoid[A]: Monoid[ControlModel[A]] = new Monoid[ControlModel[A]] {
    override def empty: ControlModel[A] = ControlModel.empty

    override def combine(x: ControlModel[A], y: ControlModel[A]): ControlModel[A] =
      ControlModel(
        x.constants ++ y.constants,
        x.fluents ++ y.fluents,
        x.controls ++ y.controls,
        x.dynamics ++ y.dynamics,
        x.globalConstraints ++ y.globalConstraints,
        x.initConstraints ++ y.initConstraints,
        x.finalConstraints ++ y.finalConstraints
      )
  }

  implicit val functor: Functor[ControlModel] = new Functor[ControlModel] {
    override def map[A, B](fa: ControlModel[A])(f: A => B): ControlModel[B] =
      ControlModel(
        fa.constants.map(c => Constant(c.name, c.tpe, f(c.value))),
        fa.fluents,
        fa.controls,
        fa.dynamics.map { case Dynamic(fluent, value) => Dynamic(fluent, f(value)) },
        fa.globalConstraints.map(f),
        fa.initConstraints.map(f),
        fa.finalConstraints.map(f)
      )
  }
}
