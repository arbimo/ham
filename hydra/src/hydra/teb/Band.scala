package hydra.teb

import cats.{collections, Applicative}
import cats.collections.Discrete
import cats.implicits._
import cats.kernel.Order
import ham.errors._
import ham.state.State
import hydra.SpireCompiler.Compilable
import hydra.{Dt, StateVariable, StateVariableInNextState, Variable}
import hydra.optim._
import spire.math.Interval

import scala.annotation.tailrec

sealed abstract class Band {
  def constraints: Seq[Compilable]
}

object Band {

  final case class Instantaneous(constraints: Seq[Compilable])                       extends Band
  final case class Durative(constraints: Seq[Compilable], dynamics: Seq[Compilable]) extends Band

}

class Problem(state: State, bands: Seq[Band]) {
  require(bands.nonEmpty)

  val layout: Variable => Option[Int] = {
    case StateVariable(name)            => state.offset(name)
    case Dt                             => Some(state.numFields)
    case StateVariableInNextState(name) => state.offset(name).map(_ + state.numFields + 1)
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

      // builds an index such that mapping(i)
      // - is the first continuous time of the ith discrete time
      // - is the last continuous time of the (i-1)th discrete time
      val mapping = go(bands.toList, 0).toArray
      def go(l: List[Band], next: Int): List[Int] = {
        l match {
          case (_: Band.Instantaneous) :: tail => next :: go(tail, next)
          case (_: Band.Durative) :: tail      => next :: go(tail, next + internalOfNonInstantaneous)
          case Nil                             => next :: Nil
        }
      }

      override def timesOf(dt: DiscreteTime): collections.Range[Int] =
        collections.Range(mapping(dt.i), mapping(dt.i + 1))
      override def discreteTimes(t: Int): Set[DiscreteTime] = ???
      override def discreteTimes: collections.Range[DiscreteTime] =
        cats.collections.Range(DiscreteTime(0), DiscreteTime(bands.length - 1))
      override def continuousTimes: collections.Range[Int] =
        collections.Range(0, timesOf(discreteTimes.end).end)
    }

  def specialize(diffFun: DiffFun, continuousTime: Int): DiffFun = {
    // state size with a space left for time delay
    val stateSize = state.numFields + 1
    DiffFun(diffFun.bridge.shiftRight(continuousTime * stateSize), diffFun.impl)
  }

  def format(sol: Array[Double], htime: HybridTime[DiscreteTime, Int]): String = {
    val sb        = new StringBuilder
    val stateSize = state.numFields + 1
    for(ctime <- htime.continuousTimes) {
      val start = ctime * stateSize
      val end   = (ctime + 1) * stateSize

      sb.append(sol.slice(start, end).map("%1.2f".format(_)).mkString("\t"))
      sb.append("\n")
    }
    sb.toString()
  }

  def solve: Attempt[RMemory] = {
    val htime = time(5)

    val x = htime.discreteTimes.toList
      .traverse(dt =>
        constraintsOnBand(bands(dt.i), layout).map {
          case (insts, dyns) =>
            val ctimes = htime.timesOf(dt).toList
            println(ctimes)
            val instaneous = ctimes.map(ctime => insts.map(specialize(_, ctime)))
            val dynamics   = ctimes.dropRight(1).map(ctime => dyns.map(specialize(_, ctime)))
            instaneous ++ dynamics
      })
      .map(_.flatten.flatten)
    val numVariables = (htime.continuousTimes.end + 1) * (state.numFields + 1)
    println("")
    x.map { dfs =>
      val ls    = new LeastSquares(dfs, numVariables)
      val sol   = Array.fill[Double](numVariables)(0.1)
      val stats = ls.lmIteration(sol, 1000)
//      val sol = ls.solveLinear
      println(s"Stats: ${stats}")
      println(format(sol, htime))
      sol
    }

  }

//  def solveLinear: Unit = {
//
//    val ls  = new LeastSquares(constraints.unsafeGet, numVars)
//    val res = ls.solveLinear
//    println(res.mkString(", "))
//    for(si <- bands.indices) {
//      for(fi <- 0 until state.numFields) {
//        println(state.fields.toArray.apply(fi).name + ": " + res(si * state.numFields + fi))
//      }
//    }
//  }

}

class DiscreteTime(val i: Int) extends AnyVal {
  def succ: DiscreteTime = new DiscreteTime(i + 1)
  def pred: DiscreteTime = new DiscreteTime(i - 1)

  def to(last: DiscreteTime): Iterator[DiscreteTime] =
    (i to last.i).iterator.map(new DiscreteTime(_))
}
object DiscreteTime {
  def apply(i: Int): DiscreteTime = new DiscreteTime(i)

  implicit val discrete: Discrete[DiscreteTime] = new Discrete[DiscreteTime] {
    override def succ(x: DiscreteTime): DiscreteTime = x.succ
    override def pred(x: DiscreteTime): DiscreteTime = x.pred
  }
  implicit val order: Order[DiscreteTime] =
    (x: DiscreteTime, y: DiscreteTime) => Order[Int].compare(x.i, y.i)
}
//class ContTime(val t: Int) extends AnyVal

trait HybridTime[D, C] {

  def timesOf(dt: D): cats.collections.Range[C]
  def discreteTimes(t: C): Set[D]
  def discreteTimes: cats.collections.Range[D]
  def continuousTimes: cats.collections.Range[C]
}
