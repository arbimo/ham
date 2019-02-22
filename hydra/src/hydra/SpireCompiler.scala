package hydra

import cats.Applicative
import com.stripe.rainier.compute
import ham.errors._
import ham.expr.{Expr, Id}
import ham.state.State
import hydra.SpireCompiler.{Compilable, RainierExprCompiler}
import hydra.compile.{FunN, Rainier}
import hydra.optim.{Bridge, DiffFun}
import spire.algebra._
import spire.implicits._
import spire.math._
import com.stripe.rainier.{compute => rainier}

object SpireCompiler {

  implicit val jetDoubleOrder = new Order[Jet[Double]] {
    override def compare(x: Jet[Double], y: Jet[Double]): Int =
      spire.implicits.DoubleAlgebra.compare(x.real, y.real)
  }

  def builtIns[@specialized(Double) T: Field: Trig: Order](name: String): Option[Any] = {
    val F: Field[T] = Field[T]
    val T: Trig[T]  = Trig[T]
    val O: Order[T] = Order[T]
    Option(
      name match {
        // format: off
        case "real.mul" => (x: T) => (y: T) => F.times(x, y)
        case "real.add" => (x: T) => (y: T) => F.plus(x, y)
        case "real.sub" => (x: T) => (y: T) => F.minus(x, y)
        case "real.min" => (x: T) => (y: T) => O.min(x, y)
        case "real.max" => (x: T) => (y: T) => O.max(x, y)
        case "real.abs" => (x: T) => O.max(x, F.negate(x))
        case "real.cos" => (x: T) => T.cos(x)
        case "real.sin" => (x: T) => T.sin(x)
        case "real.PI" => F.fromDouble(math.Pi)
        //format: on
      }
    )
  }

  def compile[Ctx, Expr](e: Expr,
                           ofSym: Id => Option[Either[Ctx => Any, Expr]],
                           builtIn: String => Option[Any],
                           litPrepro: Any => Any): Attempt[Ctx => Any] =
    ham.errors.attempt {
      compileUnsafe(e, ofSym, builtIn, litPrepro)
    }

  def compileUnsafe[Ctx, Expr](e: Expr,
                           ofSym: Id => Option[Either[Ctx => Any, Expr]],
                           builtIn: String => Option[Any],
                           litPrepro: Any => Any): Ctx => Any = e match {
    case ham.expr.Literal(x, _) =>
      (_: Ctx) =>
        litPrepro(x)
    case ham.expr.Fun(Nil, body) => compileUnsafe(body, ofSym, builtIn, litPrepro)
    case ham.expr.Fun(_, body)   => ???
    case ham.expr.Var(_)         => ???
    case ham.expr.Symbol(id) =>
      ofSym(id) match {
        case None           => throw ham.errors.error(s"Unknown symbol: $id")
        case Some(Left(f))  => f
        case Some(Right(e)) => compileUnsafe(e, ofSym, builtIn, litPrepro)
      }
    case ham.expr.BuiltIn(name, _) =>
      builtIn(name) match {
        case Some(v) =>
          (_: Ctx) =>
            v
        case None => throw ham.errors.error(s"Unknown built in $name")
      }
    case ham.expr.App(fun, arg) =>
      val funPE = compileUnsafe(fun, ofSym, builtIn, litPrepro).asInstanceOf[Ctx => Any => Any]
      val argPE = compileUnsafe(arg, ofSym, builtIn, litPrepro)
      (s: Ctx) =>
        funPE(s)(argPE(s))
  }

  def evaluator(c: Expr, s: State, defs: Id => Option[Expr]): Array[Double] => Double = {
    val res = compile[Array[Double], Expr](
        c,
        id => {
          defs(id) match {
            case Some(e) =>
              Some(Right(e))

            case None =>
              s.offset(id.local)
                  .map(i => Left((arr: Array[Double]) => arr(i)))
          }
        },
        builtInName => builtIns[Double](builtInName),
        x => x
      )
    res.asInstanceOf[Array[Double] => Double]
  }

  def differentiatorUnsafe(c: Expr, s: State, defs: Id => Option[Expr]): Array[Jet[Double]] => Jet[Double] = {
    implicit val jetDim = JetDim(s.numFields)

    val differentiator = compileUnsafe[Array[Jet[Double]], Expr](
      c,
      id => {
        defs(id) match {
          case Some(e) =>
            Some(Right(e))

          case None =>
            s.offset(id.local)
              .map(i => Left((values: Array[Jet[Double]]) => values(i)))
        }
      },
      name => builtIns[Jet[Double]](name), {
        case d: Double => Jet(d)
      }
    )
    differentiator.asInstanceOf[Array[Jet[Double]] => Jet[Double]]
  }

  trait Compilable {

    def compile(stateReader: Variable => Option[Int], dim: Int): Attempt[DiffFun]
  }


  object RainierExprCompiler {
  }

}

object RainierCompiler {
  /** Global cache of rainier variables.
    * This is used to make sure that refering to ith variable when compiling refers to the
    * same variable in independently compile expressions
    * */ 
  private val vars = Array.fill(1000)(new rainier.Variable)
  private def getVariable(i: Int): rainier.Variable = vars(i)
  private def getVariableVector(num: Int): Array[rainier.Variable] = vars.take(num)

  type ToReal = (Variable => Option[Int]) => Attempt[compute.Real]

  def compilable(x: ToReal): Compilable =
    (binds: Variable => Option[Int],                          dim: Int) => compile(x, binds, dim)


  def compile(x: ToReal, binds: Variable => Option[Int], dim: Int): Attempt[DiffFun] = {
      x(binds).map { real =>
        // todo: change the bridge to only use variables we are interested in
        val funn = Rainier.compile(getVariableVector(dim), real)
        DiffFun(Bridge.identity(dim), funn)
      }
    }

  def toReal(defs: Id => Option[Expr])(c: Expr): ToReal = new ExprCompiler(c, defs)
  def svInCurrentState(name: String): ToReal = (binds: Variable => Option[Int]) =>
    binds(StateVariable(name))
      .map(i => getVariable(i))
      .toAttemptMsg(s"Non accessible in next state: $name")

  def svInNextState(name: String): ToReal = (binds: Variable => Option[Int]) =>
      binds(StateVariableInNextState(name))
        .map(i => getVariable(i))
        .toAttemptMsg(s"Non accessible in next state: $name")
  def dt: ToReal = (binds: Variable => Option[Int]) =>
      binds(Dt)
        .map(i => getVariable(i))
        .toAttemptMsg("Dt not accessible")

  private class ExprCompiler(c: Expr, defs: Id => Option[Expr]) extends ToReal {

    def apply(stateReader: Variable => Option[Int]): Attempt[rainier.Real] =
      Rainier.compile(c,
        id => {
          defs(id) match {
            case Some(e) =>
              Some(Right(e))

            case None =>
            stateReader(StateVariable(id.local))
              .map(i => Left(getVariable(i)))
          }
        }
      )
  }
}

sealed abstract class Variable
final case class StateVariable(name: String) extends Variable
final case object Dt extends Variable
final case class StateVariableInNextState(name: String) extends Variable
