package hydra.teb

import cats.{collections, Applicative}
import cats.collections.Discrete
import cats.implicits._
import ham.errors._
import ham.state.State
import hydra.Compiler.Compilable
import hydra.{StateVariable, Variable}
import hydra.optim._
import spire.math.Interval

sealed abstract class Band {
  def constraints: Seq[Compilable]
}

object Band {

  final case class Instantaneous(constraints: Seq[Compilable])                       extends Band
  final case class Durative(constraints: Seq[Compilable], dynamics: Seq[Compilable]) extends Band

}

class Problem(state: State, bands: Seq[Band]) {
  require(bands.nonEmpty)

  def numVars: Int = state.numFields * bands.size

  def constraints: Attempt[List[DiffFun]] = {
    bands.indices
      .flatMap(i => {
        bands(i).constraints.map { compilable =>
          compilable
            .compile({
              case StateVariable(name) =>
                state.offset(name)
              case _ => None
            }, state.numFields)
            .map {
              case DiffFun(bridge, impl) => DiffFun(bridge.shiftRight(i * state.numFields), impl)
            }
        }
//      bands(i).constraints.map {
//        case DiffFun(bridge, impl) => DiffFun(bridge.shiftRight(i * state.numFields), impl)
//      }
      })
      .toList
      .sequence
  }

  def constraintsOnBand(
      b: Band,
      layout: Variable => Option[Int]): Attempt[(List[DiffFun], List[DiffFun])] = {
    val instantaneousLayout: Variable => Option[Int] = {
      case x: StateVariable => layout(x)
      case _                => None
    }
    val inst = b.constraints.toList
      .traverse(_.compile(instantaneousLayout, state.numFields))
    val dyns: Attempt[List[DiffFun]] = b match {
      case Band.Instantaneous(_) => Succ(Nil)
      case Band.Durative(_, xs) =>
        xs.toList.traverse(_.compile(layout, state.numFields * 2 + 1))
    }
    Applicative[Attempt].product(inst, dyns)
  }

  def time(internalOfNonInstantaneous: Int): HybridTime[DiscreteTime, Int] =
    new HybridTime[DiscreteTime, Int] {

      private def go(l: List[Band], next: Int): List[Int] = {
        l match {
          case (_: Band.Instantaneous) :: tail => next :: go(tail, next)
          case (_: Band.Durative) :: tail      => next :: go(tail, next + internalOfNonInstantaneous)
          case Nil                             => next :: Nil
        }
      }
      val mapping = go(bands.toList, 0).reverse.toArray

      override def timesOf(dt: DiscreteTime): collections.Range[Int] =
        collections.Range(mapping(dt.i), mapping(dt.i + 1))
      override def discreteTimes(t: Int): Set[DiscreteTime] = ???
    }

  def solveLinear: Unit = {

    val ls  = new LeastSquares(constraints.unsafeGet, numVars)
    val res = ls.solveLinear
    println(res.mkString(", "))
    for(si <- bands.indices) {
      for(fi <- 0 until state.numFields) {
        println(state.fields.toArray.apply(fi).name + ": " + res(si * state.numFields + fi))
      }
    }
  }

}

class DiscreteTime(val i: Int) extends AnyVal {
  def succ: DiscreteTime = new DiscreteTime(i + 1)
  def pred: DiscreteTime = new DiscreteTime(i - 1)

  def to(last: DiscreteTime): Iterator[DiscreteTime] =
    (i to last.i).iterator.map(new DiscreteTime(_))
}
object DiscreteTime {
  implicit val discrete: Discrete[DiscreteTime] = new Discrete[DiscreteTime] {
    override def succ(x: DiscreteTime): DiscreteTime = x.succ
    override def pred(x: DiscreteTime): DiscreteTime = x.pred
  }
}
//class ContTime(val t: Int) extends AnyVal

trait HybridTime[D, C] {

  def timesOf(dt: D): cats.collections.Range[C]
  def discreteTimes(t: C): Set[D]
}
