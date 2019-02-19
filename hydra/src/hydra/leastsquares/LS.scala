package hydra.leastsquares

import cats.implicits._
import hydra.Compiler
import hydra.optim._
import ham.errors.Attempt
import ham.expr.{Expr, Id}
import ham.prelude.Prelude
import ham.state.{State, StateField}
import hydra.optim.DiffFun

import scala.collection.immutable.ListMap

object LS {

  val prelude = Prelude.typedPrelude

  def parse(source: String): Attempt[LSModel[Expr]] = {
    for {
      mod <- LSParser.parse(source)
      syms = prelude.mod.unqualifiedNames ++ mod.symbols
      modExpr <- mod.mapErr(x => Expr.fromAST(x, id => syms.get(id)))
    } yield modExpr
  }

  def extract(mod: LSModel[Expr]): Attempt[(State, Seq[DiffFun])] = {
    val fields     = mod.variables.map(f => StateField.real(f.name))
    val stateShape = new State(fields.toArray)
    val adap       = stateShape.arrayRep

    val csts                     = mod.constants.map(c => mod.moduleID / c.name -> c.value).toMap
    val defs: Id => Option[Expr] = id => csts.get(id).orElse(prelude.mod.definition(id).toOption)

    def constraintsToFun(constraints: List[Expr]): List[DiffFun] =
      constraints
        .map(c => Compiler.differentiator(c, stateShape, defs))
        .map(f => {
          val dfi = new DiffFunImpl(stateShape.numFields, f)
          val df  = new DiffFun(Bridge.identity(stateShape.numFields), dfi)
          df
        })
    val errors = constraintsToFun(mod.constraints)
    ham.errors.success((stateShape, errors))
  }

  def solveLinear(mod: LSModel[Expr]): Attempt[Map[String, Double]] = {
    extract(mod).map {
      case (stateShape, errors) =>
        val ls  = new LeastSquares(errors, stateShape.numFields)
        val sol = ls.solveLinear

        stateShape.fields.zipWithIndex.foldLeft(ListMap.empty[String, Double]) {
          case (m, (f, i)) => m.updated(f.name, sol(i))
        }
    }
  }

  def solveNonlinear(mod: LSModel[Expr]): Attempt[Map[String, Double]] = {
    extract(mod).map {
      case (stateShape, errors) =>
        val ls = new LeastSquares(errors, stateShape.numFields)

        val s0 = Array.fill[Double](stateShape.numFields)(0.0001)
        ls.lmIteration(s0, 100)

        val sol = s0

        stateShape.fields.zipWithIndex.foldLeft(ListMap.empty[String, Double]) {
          case (m, (f, i)) => m.updated(f.name, sol(i))
        }
    }
  }
}
