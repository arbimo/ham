package hydra.teb

import cats.{collections, Applicative}
import cats.implicits._
import ham.errors._
import ham.state.State
import hydra.SpireCompiler.Compilable
import hydra._
import hydra.memory.{ArraySlice, DoubleLike, StateSequence, StateWriter}
import hydra.optim._

import scala.annotation.tailrec

sealed abstract class Band {
  def constraints: Seq[Compilable]
}

object Band {

  final case class Instantaneous(constraints: Seq[Compilable])                       extends Band
  final case class Durative(constraints: Seq[Compilable], dynamics: Seq[Compilable]) extends Band

}

class Problem(state: State, bands: Seq[Band]) { problem =>
  require(bands.nonEmpty)
  val schema = new hydra.memory.Schema(state.numFields)
  val writer = new StateWriter(
    (state.fields.map(_ => DoubleLike.OfDouble).toSeq :+ DoubleLike.StrictlyPositive).toArray
  )

  val layout: Variable => Option[Int] = {
    case StateVariable(name)            => state.offset(name)
    case Dt                             => Some(state.numFields)
    case StateVariableInNextState(name) => state.offset(name).map(_ + state.numFields + 1)
  }

  val constraintsPerBand: Array[(List[DiffFun], List[DiffFun])] = {
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
    bands.toList.traverse(b => constraintsOnBand(b, layout)).unsafeGet.toArray
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

  /** Specializes a given function expressed on the first state to apply to an arbitrary state */
  def specialize(diffFun: DiffFun, continuousTime: Int): DiffFun = {
    // state size with a space left for time delay
    val stateSize = state.numFields + 1
    DiffFun(diffFun.bridge.shiftRight(continuousTime * stateSize), diffFun.impl)
  }

  def constraints(htime: HybridTime[DiscreteTime, Int]) = {
    htime.discreteTimes.toList
      .flatMap(dt =>
        constraintsPerBand(dt.i) match {
          case (insts, dyns) =>
            val ctimes     = htime.timesOf(dt).toList
            val instaneous = ctimes.map(ctime => insts.map(specialize(_, ctime)))
            val dynamics   = ctimes.dropRight(1).map(ctime => dyns.map(specialize(_, ctime)))
            instaneous ++ dynamics
      })
      .flatten
  }

  type HTime = HybridTime[DiscreteTime, Int]

  sealed trait RunResult
  case class Success(htime: HTime, vals: StateSequence) extends RunResult
  case class Ran(htime: HTime, vals: StateSequence)     extends RunResult
  case class Failed()                                   extends RunResult

  sealed trait Policy {
    def makeRun(previous: Ran): RunResult
  }
  case class FitTime(targetDt: Double) {

    /** Generic constraint to apply on all Dt variables. */
    private val onDts: DiffFun = {
      import hydra.algebra._
      val N      = Num[RainierCompiler.ToReal]
      val toReal = N.minus(RainierCompiler.dt, N.fromDouble(targetDt))
      RainierCompiler.compilable(toReal).compile(layout, state.numFields + 1).unsafeGet
    }
    private def constraintsWithTime(htime: HTime) = {
      problem.constraints(htime) ++ htime.continuousTimes.toList
        .dropRight(1)
        .map(i => specialize(onDts, i))
    }

    def makeRun(previous: Ran): RunResult = {
      val base     = previous.vals
      val prevN    = previous.htime.continuousTimes.end + 1
      val dtRatio  = schema.averageTime(base.slice) / targetDt
      val desiredN = (prevN * dtRatio).toInt
      if(prevN == desiredN || (dtRatio - 1).abs < 0.01) {
        Success(previous.htime, previous.vals)
      } else {
        // compute the next number of states
        val up    = math.min(desiredN, prevN * 2)
        val n     = math.max(math.max(prevN / 2, up), 2)
        val htime = time(n)

        // produce new solution shape and populate it from the incumbent
        val curr = schema.init(htime.continuousTimes.end + 1)
        schema.writeHomogenized(ArraySlice(base.raw), curr)

        val incumbent = writer.init(curr.mem)

        // run least squares
        val constraints = this.constraintsWithTime(htime)
        val ls          = new LeastSquares(constraints, curr.length, writer)

        val stats = ls.lmIteration(incumbent, 20)
        Ran(htime, incumbent)
      }
    }

  }

  def solve(targetDt: Double): Attempt[Solution] = {
    val policy = FitTime(targetDt)
    val base   = Ran(time(1), writer.init((state.numFields + 1) * 3))

    @tailrec def go(previous: Ran): Attempt[Solution] = {
      policy.makeRun(previous) match {
        case Success(htime, vals) =>
          ham.errors.success(new Solution(state, htime, vals))
        case Failed()             => ham.errors.failure("Failed")
        case x @ Ran(htime, vals) =>
          // method was run repeat until we get a success or failure
          go(x)
      }
    }
    go(base)
  }
}
