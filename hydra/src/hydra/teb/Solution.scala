package hydra.teb

import ham.state.State
import cats.implicits._
import hydra.memory.StateSequence

class Solution(state: State, htime: HybridTime[DiscreteTime, Int], evolution: StateSequence) {

  def format(sep: String = " "): String = {
    val sb                              = new StringBuilder
    @inline def padd(s: String): String = String.format(s"%-12s", s)
    @inline def write(s: String): Unit  = { sb.append(padd(s)); sb.append(sep) }
    @inline def dwrite(s: Double): Unit = { write("%1.2f".format(s)) }
    @inline def newLine(): Unit         = { sb.append("\n") }

    val stateSize = state.numFields + 1
    write("time")
    for(f <- state.fields) {
      write(f.name)
    }
    write("(tnext - t)")
    newLine()
    var t: Double = 0
    for(ctime <- htime.continuousTimes) {
      dwrite(t)
      t += evolution(ctime * stateSize + stateSize - 1)
      val start = ctime * stateSize
      val end   = (ctime + 1) * stateSize
      var i     = start
      while(i < end) {
        dwrite(evolution(i))
        i += 1
      }
      newLine()
    }
    sb.toString()
  }
}
