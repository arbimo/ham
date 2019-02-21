package hydra.control

import cats.implicits._
import ham.errors._
import ham.expr.{Expr, Id, Type}
import ham.lang.Env
import ham.state.{State, StateField}
import ham.typing.Typer
import hydra.Compiler.{Compilable, ExprCompiler}
import hydra.reductions.Reductions
import hydra.teb.{Band, Problem}

object Controller {

  import ham.prelude.Prelude._

  def constraintToError(constraint: Expr, s: State)(implicit env: Env): Attempt[Compilable] = {
    val tpe              = Typer.typeOf(constraint, env.typeOf)
    val defs: Id => Expr = env.definitionOf(_).unsafeGet
    tpe match {
      case Succ(Bool) =>
        val e = Reductions.bool2err(constraint, Nil, defs)
        ham.errors.attempt {
          new ExprCompiler(e, env.definitionOf(_).toOption)
//          val diff = hydra.Compiler.differentiator(e, s, env.definitionOf(_).toOption)
//          DiffFun(Bridge.identity(s.numFields), new DiffFunImpl(s.numFields, diff))
        }

      case Succ(tpe) =>
        ham.errors.failure(s"Constraint $constraint has type $tpe but expected $Bool")
      case fail @ Fail(_) => fail

    }
  }

  def stateSchema(model: ControlModel[_]): State = {
    val fields = (model.fluents.map(_.name) ++ model.controls.map(_.name)).map(StateField.real(_))
    new State(fields.toArray)
  }

  def toTeb(modExpr: ControlModel[Expr]): Attempt[hydra.teb.Problem] = {

    val csts                     = modExpr.constants.map(c => modExpr.moduleID / c.name -> c.value).toMap
    val types                    = modExpr.types ++ typedPrelude.types
    val defs: Id => Option[Expr] = id => csts.get(id).orElse(prelude.definition(id).toOption)

    val schema = stateSchema(modExpr)

    implicit val env = new Env {
      override def typeOf(id: Id): Attempt[Type] =
        types.get(id).toAttemptMsg(s"Unkonwn symbol $id")
      override def definitionOf(id: Id): Attempt[Expr] =
        defs(id).toAttemptMsg(s"Unkonwn symbol $id")
    }

    val startBand =
      (modExpr.initConstraints ++ modExpr.globalConstraints) //.map(constraintToError(_, schema))

    val endBand =
      (modExpr.finalConstraints ++ modExpr.globalConstraints) //.map(constraintToError(_, schema))

    def constraitnsToBand(constraints: List[Expr]): Attempt[Band] = {

      for {
        errors <- constraints.map(constraintToError(_, schema)).sequence
      } yield {
        hydra.teb.Band.Instantaneous(errors)
      }

    }

    val constraints = List(startBand, endBand)
    val pb = for {
      bands <- constraints.map(constraitnsToBand(_)).sequence
    } yield {
      val X = new Problem(schema, bands)
      X.solveLinear
      X
    }

    pb
  }

}
