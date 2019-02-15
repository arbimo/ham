package ham.matrix

final class Vector(data: Array[Double]) {
  def this(size: Int) = this(new Array[Double](size))

  val size                            = data.length
  def apply(i: Int): Double           = data(i)
  def update(i: Int, v: Double): Unit = data(i) = v
}
