package hydra

import ham.expr.{Expr, Id}
import ham.state.State
import spire.algebra._
import spire.implicits._
import spire.math._

object Compiler {

  implicit val jetDoubleOrder = new Order[Jet[Double]] {
    override def compare(x: Jet[Double], y: Jet[Double]): Int =
      Order[Double].compare(x.real, y.real)
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
                           litPrepro: Any => Any): Ctx => Any = e match {
    case ham.expr.Literal(x, _) =>
      (_: Ctx) =>
        litPrepro(x)
    case ham.expr.Fun(Nil, body) => compile(body, ofSym, builtIn, litPrepro)
    case ham.expr.Fun(_, body)   => ???
    case ham.expr.Var(_)         => ???
    case ham.expr.Symbol(id) =>
      ofSym(id) match {
        case None           => sys.error(s"Unknown symbol: $id")
        case Some(Left(f))  => f
        case Some(Right(e)) => compile(e, ofSym, builtIn, litPrepro)
      }
    case ham.expr.BuiltIn(name, _) =>
      builtIn(name) match {
        case Some(v) =>
          (_: Ctx) =>
            v
        case None => sys.error(s"Unknown built in $name")
      }
    case ham.expr.App(fun, arg) =>
      val funPE = compile(fun, ofSym, builtIn, litPrepro).asInstanceOf[Ctx => Any => Any]
      val argPE = compile(arg, ofSym, builtIn, litPrepro)
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

  def differentiator(c: Expr, s: State, defs: Id => Option[Expr]): Array[Jet[Double]] => Jet[Double] = {
    implicit val jetDim = JetDim(s.numFields)

    val differentiator = compile[Array[Jet[Double]], Expr](
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
}
