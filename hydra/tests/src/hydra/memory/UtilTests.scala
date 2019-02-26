package hydra.memory

import minitest._

object UtilTests extends SimpleTestSuite {

  test("homogenization") {
    val stateSize = 2
    val in        = Array[Double](0, 0, 1, 1, 1)
    val out       = Array.fill[Double](3 * (stateSize + 1) - 1)(0)
    hydra.memory.Utils.homogenize(in, 0, out, 0, stateSize, 2, 3)
    assert(out.toSeq == Array[Double](0, 0, 0.5, 0.5, 0.5, 0.5, 1, 1).toSeq)
  }

  test("homogenization with offsets") {
    val stateSize = 2
    val in = Array[Double](Double.NaN,
                           Double.NaN,
                           Double.NaN,
                           0,
                           0,
                           1,
                           1,
                           1,
                           Double.NaN,
                           Double.NaN,
                           Double.NaN)
    val out = Array.fill[Double](5 * (stateSize + 1) - 1)(Double.NaN)
    hydra.memory.Utils.homogenize(in, 3, out, 6, stateSize, 2, 3)
    assert(
      out.toSeq.slice((stateSize + 1) * 2, out.length)
        == Seq[Double](0, 0, 0.5, 0.5, 0.5, 0.5, 1, 1))
  }

}
