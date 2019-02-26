package hydra.memory

class StateSequence(mem: Array[Double], writer: StateWriter) {

  val size: Int = mem.length
  val width     = writer.writers.length

  def apply(i: Int): Double = mem(i)
  def update(i: Int, v: Double): Unit = {
    mem(i) = writer(i).normalize(v)
  }

  def raw: Array[Double] = mem

  /** Increment the state with provided values, treating NaN inputs as 0. */
  def incrementGuardedNaN(increments: Array[Double]): Unit = {
    require(increments.length == size, s"Increments length does not match memory length")
    var i = 0
    while(i < increments.length) {
      val inc = increments(i)
      if(!inc.isNaN)
        update(i, mem(i) + increments(i))
      i += 1
    }
  }

  def load(values: Array[Double]): Unit = {
    require(values.length == mem.length)
    var i = 0
    while(i < size) {
      val v = values(i)
      if(v.isNaN)
        throw new AssertionError("Trying to load a NaN into a state sequence")
      mem(i) = v
      i += 1
    }
  }

  def slice: ArraySlice = ArraySlice(mem)

  override def clone(): StateSequence = new StateSequence(mem.clone(), writer)

}

abstract class DoubleLike {
  val default: Double
  def normalize(in: Double): Double
}
object DoubleLike {

  object OfDouble extends DoubleLike {
    override val default: Double               = 0
    override def normalize(in: Double): Double = in
  }
  object StrictlyPositive extends DoubleLike {
    private val epsilon: Double  = 1e-08
    override val default: Double = epsilon
    override def normalize(in: Double): Double = {
      if(in > 0)
        in
      else
        epsilon
    }
  }
}

final class StateWriter(val writers: Array[DoubleLike]) {
  private val width = writers.length

  def apply(i: Int): DoubleLike = writers(i % writers.length)

  def init(base: Array[Double]): StateSequence = {
    new StateSequence(base.clone(), this)
  }

  def init(size: Int): StateSequence = {
    val pattern = {
      val tmp = new Array[Double](width)
      var i   = 0
      while(i < width) {
        tmp(i) = apply(i).default
        i += 1
      }
      tmp
    }

    val mem = new Array[Double](size)
    var i   = 0
    while(i < size) {
      mem(i) = pattern(i % width)
      i += 1
    }
    new StateSequence(mem, this)
  }

}
object StateWriter {
  def apply(writers: DoubleLike*): StateWriter = new StateWriter(writers.toArray)
}
