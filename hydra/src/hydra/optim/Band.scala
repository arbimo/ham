package hydra.optim

import ham.state.State
import hydra.optim.Band.Instantaneous

sealed abstract class Band {
  def constraints: Seq[DiffFun]
}

object Band {

  final case class Instantaneous(constraints: Seq[DiffFun]) extends Band





}

class Problem(state: State, bands: Seq[Band]) {
  require(bands.size > 0)


  def numVars: Int = state.numFields * bands.size

  def constraints: Seq[DiffFun] = {
    bands.indices.flatMap(i => {
      bands(i).constraints.map {
        case DiffFun(bridge, impl) => DiffFun(bridge.shiftRight(i * state.numFields), impl)
      }
    })
  }

  def solveLinear: Unit = {

    val ls = new LeastSquares(constraints, numVars)
    val res = ls.solveLinear
    println(res.mkString(", "))
    for(si <- bands.indices) {
      for(fi <- 0 until state.numFields) {
        println(state.fields.toArray.apply(fi).name+ ": " + res(si * state.numFields + fi))
      }
    }
  }

}


