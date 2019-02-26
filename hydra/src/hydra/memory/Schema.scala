package hydra.memory

class Schema(val stateSize: Int) {
  private val width = stateSize + 1
  def numStates(a: ArraySlice): Int = {
    (a.length + 1) / (stateSize + 1)
  }

  def init(numStates: Int, fill: Double = 0.1): ArraySlice = {
    val mem = Array.fill[Double](numStates * width)(fill)
    new ArraySlice(mem, 0, mem.length)
  }

  def slice(mem: Array[Double], firstState: Int, lastState: Int): ArraySlice = {
    new ArraySlice(mem, firstState * width, (lastState - firstState) * width - 1)
  }

  def writeHomogenized(in: ArraySlice, out: ArraySlice): Unit = {
    Utils.homogenize(in.mem,
                     in.startPos,
                     out.mem,
                     out.startPos,
                     stateSize,
                     numStates(in),
                     numStates(out))
  }

  def totalTime(a: ArraySlice): Double = {
    val n   = numStates(a)
    var acc = 0.0
    var i   = 0
    while(i < n - 1) {
      acc += a(i * width + stateSize)
      i += 1
    }
    acc
  }
  def averageTime(a: ArraySlice): Double = {
    totalTime(a) / (numStates(a) - 1)
  }

  def shrinkTime(a: ArraySlice, factor: Double): Unit = {
    val n   = numStates(a)
    var acc = 0.0
    var i   = 0
    while(i < n - 1) {
      a(i * stateSize) *= factor
      i += 1
    }
    acc
  }

}
