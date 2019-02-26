package hydra.memory
import scala.reflect.ClassTag

object Utils {

  private val epsilon = 1e-8

  // type alias to facilitate type safety
  type Time <: Double
  private implicit val timeCT: ClassTag[Time] = ClassTag.Double.asInstanceOf[ClassTag[Time]]
  private object Time {
    def apply(t: Double): Time = t.asInstanceOf[Time]
  }
  type InStateIndex <: Int
  private object InStateIndex {
    def apply(i: Int): InStateIndex         = i.asInstanceOf[InStateIndex]
    def next(s: InStateIndex): InStateIndex = InStateIndex(s + 1)
    def prev(s: InStateIndex): InStateIndex = InStateIndex(s - 1)
  }
  type OutStateIndex <: Int
  private object OutStateIndex {
    def apply(i: Int): OutStateIndex = i.asInstanceOf[OutStateIndex]
  }

  /** Provides a time-homogenous view of a sequence of state through linear interpolation.
    * given in = [0, 0, 2, 1, 1] meaning the state [0,0] followed by the state [1,1] separated by dt = 2
    * Asking for three states in the output, we should get
    * [0,0, 1, 0.5, 0.5, 1, 1,1] meanings the state sequence [0,0], [0.5,0.5], [1,1] each separated by a dt = 1
    * Offset parameters determine where in the array we start reading/writing.
    * State size should not account for dt (i.e. stateSize should be two in the above exampes
    *
    * */
  def homogenize(in: Array[Double],
                 inputOffset: Int,
                 out: Array[Double],
                 outputOffset: Int,
                 stateSize: Int,
                 numInStates: Int,
                 numOutStates: Int): Unit = {
    require(numInStates >= 2)
    require(numOutStates >= 2)
    def read(state: InStateIndex, offset: Int): Double =
      in(inputOffset + state * (stateSize + 1) + offset)
    def write(state: OutStateIndex, field: Int, value: Double): Unit = {
      assert(field <= stateSize) // not lets one space for dt
      out(outputOffset + state * (stateSize + 1) + field) = value
    }
    def readDeltaTime(state: InStateIndex): Double = read(state, stateSize)
    val times: Array[Time] = {
      val times = new Array[Time](numInStates)
      var accT  = 0d
      var i     = 0
      while(i < numInStates) {
        times(i) = Time(accT)
        if(i != numInStates - 1) {
          // last one, dt not applicable
          assert(readDeltaTime(InStateIndex(i)) > 0, s"A dt is not greater than 0")
          accT += readDeltaTime(InStateIndex(i))
        }
        i += 1
      }
      times
    }
    def readTime(state: InStateIndex): Time = {
      assert(state < numInStates, s"state $state not input states")
      times(state)
    }
    val totalTime               = readTime(InStateIndex(numInStates - 1))
    val outputDeltaTime: Double = totalTime / (numOutStates - 1)
    def absTimeToState(t: Time, startFrom: InStateIndex): (InStateIndex, InStateIndex) = {
      var s = startFrom
      while(readTime(InStateIndex.next(s)) <= (t - epsilon)) {
        if(s == numInStates - 1) {
          // last state
          val st = readTime(s)
          Predef.assert((t - st).abs < epsilon)
          return (s, s)
        }
        s = InStateIndex.next(s)
        assert(s <= numInStates)
      }
      assert(readTime(InStateIndex.next(s)) >= (t - epsilon))
      (s, InStateIndex.next(s))
    }
    def linearInterpol(xPrev: Double,
                       xNext: Double,
                       tPrev: Time,
                       tNext: Time,
                       targetTime: Time): Double = {
      val dt          = tNext - tPrev
      val beforeRatio = 1 - (targetTime - tPrev) / dt
      val afterRatio  = 1 - beforeRatio
      assert(beforeRatio <= 1 + epsilon)
      assert(beforeRatio >= 0 - epsilon)
      assert(afterRatio <= 1 + epsilon)
      assert(afterRatio >= 0 - epsilon)
      xPrev * beforeRatio + xNext * afterRatio
    }
    var prevBelowState = InStateIndex(0)
    for(outState <- (0 until numOutStates).map(OutStateIndex(_))) {
      val targetTime               = Time(outState * outputDeltaTime)
      val (belowState, aboveState) = absTimeToState(targetTime, prevBelowState)
      prevBelowState = belowState // optimization, to avoid rescanning everything
      for(field <- 0 until stateSize) {
        val prev = read(belowState, field)
        val next = read(aboveState, field)
        val out  = linearInterpol(prev, next, readTime(belowState), readTime(aboveState), targetTime)
        write(outState, field, out)
      }
      if(outState != numOutStates - 1) {
        // write dt for all but last state
        write(outState, stateSize, outputDeltaTime)
      }

    }
  }

}
