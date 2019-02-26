package hydra.control

import cats.implicits._
import ham.errors._
import ham.expr.{Expr, Id, Type}
import ham.lang.Env
import ham.state.{State, StateField}
import ham.typing.Typer
import hydra.RainierCompiler
import hydra.SpireCompiler.Compilable
import hydra.reductions.Reductions
import hydra.teb.{Band, Problem}

object Controller {

  import ham.prelude.Prelude._

  /** Two steps transformation:
    *  - transforms the boolean expression in to a real-valued one encoding the error
    *  - make it compilable
    **/
  private def constraintToError(constraint: Expr, s: State, env: Env): Attempt[Compilable] = {
    val tpe              = Typer.typeOf(constraint, env.typeOf)
    val defs: Id => Expr = env.definitionOf(_).unsafeGet
    val asReal           = RainierCompiler.toReal(env.definitionOf(_).toOption)(_)
    tpe match {
      case Succ(Bool) =>
        val e = Reductions.bool2err(constraint, Nil, defs)
        Succ(
          RainierCompiler.compilable(asReal(e))
        )

      case Succ(tpe) =>
        ham.errors.failure(s"Constraint $constraint has type $tpe but expected $Bool")
      case fail @ Fail(_) => fail

    }
  }

  private def stateSchema(model: ControlModel[_]): State = {
    val fields = (model.fluents.map(_.name) ++ model.controls.map(_.name)).map(StateField.real(_))
    new State(fields.toArray)
  }

  def toTeb(modExpr: ControlModel[Expr]): Attempt[hydra.teb.Problem] = {

    val csts                     = modExpr.constants.map(c => modExpr.moduleID / c.name -> c.value).toMap
    val types                    = modExpr.types ++ typedPrelude.types
    val defs: Id => Option[Expr] = id => csts.get(id).orElse(prelude.definition(id).toOption)

    val schema = stateSchema(modExpr)

    val env = new Env {
      override def typeOf(id: Id): Attempt[Type] =
        types.get(id).toAttemptMsg(s"Unknown symbol $id")
      override def definitionOf(id: Id): Attempt[Expr] =
        defs(id).toAttemptMsg(s"Unknown symbol $id")
    }

    // start and end constraints will be mixed with the global constraints in the middle
    // because the are on instantaneous bands that overlap with the previous/next ones
    val startConstraints  = modExpr.initConstraints
    val middleConstraints = modExpr.globalConstraints
    val endConstraints    = modExpr.finalConstraints

    def constraintsToErrors(constraints: List[Expr]): Attempt[List[Compilable]] =
      constraints.traverse(constraintToError(_, schema, env))

    import hydra.algebra._
    val N = Num[RainierCompiler.ToReal]

    def dyn2Error(dyn: Dynamic[Expr]): Compilable = {
      val sv  = dyn.fluent
      val rhs = RainierCompiler.toReal(env.definitionOf(_).toOption)(dyn.value)

      val lhs = N.div(
        N.minus(RainierCompiler.svInNextState(sv), RainierCompiler.svInCurrentState(sv)),
        RainierCompiler.dt)
      val result = N.minus(rhs, lhs)
      RainierCompiler.compilable(result)
    }

    val pb = for {
      startBand    <- constraintsToErrors(startConstraints).map(Band.Instantaneous)
      middleErrors <- constraintsToErrors(middleConstraints)
      middleDynErrors = modExpr.dynamics.map(dyn2Error)
      middleBand      = Band.Durative(middleErrors, middleDynErrors)
      endBand <- constraintsToErrors(endConstraints).map(Band.Instantaneous)
    } yield {
      new Problem(schema, List(startBand, middleBand, endBand))
    }
    pb
  }

}
