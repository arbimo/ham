package hydra.compile

import spire.math.{Jet, JetDim}

abstract class FunN(val inputSize: Int, val outputSize: Int) {
  final def eval(input: Array[Double]): Array[Double] = {
    val out = new Array[Double](outputSize)
    writeEvalRange(input, 0, outputSize - 1, out)
    out
  }
  final def evalRange(input: Array[Double], first: Int, last: Int): Array[Double] = {
    val out = new Array[Double](last - first + 1)
    writeEvalRange(input, first, last, out)
    out
  }

  def evalOne(input: Array[Double], n: Int): Double

  def writeEvalRange(input: Array[Double], first: Int, last: Int, output: Array[Double]): Unit = {
    var i = first
    while(i <= last) {
      output(i - first) = evalOne(input, i)
      i += 1
    }
  }
}
object FunN {
  def byNumericDiff(arity: Int, f: Array[Double] => Double): FunN     = new NumericDiff(f, arity)
  def fromJet(arity: Int, f: Array[Jet[Double]] => Jet[Double]): FunN = new FromJet(arity, f)
}

final class FunNExplicit(inputSize: Int, parts: Array[Array[Double] => Double])
    extends FunN(inputSize, parts.length) {

  def evalOne(input: Array[Double], n: Int): Double = {
    parts(n).apply(input)
  }
}

final class NumericDiff(f: Array[Double] => Double, numParams: Int)
    extends FunN(numParams, numParams + 1) {
  override def evalOne(input: Array[Double], n: Int): Double = {
    if(n == 0) {
      f(input)
    } else {
      // store to make sure me do not modify it due to rounding errors
      val x  = input(n - 1)
      val dx = 10E-6
      input(n - 1) = x + dx
      val f2 = f(input)
      input(n - 1) = x - dx
      val f1   = f(input)
      val grad = (f2 - f1) / (dx * 2)
      // restore input vector
      input(n - 1) = x
      grad
    }
  }
}

final class FromJet(arity: Int, f: Array[Jet[Double]] => Jet[Double])
    extends FunN(arity, arity + 1) {

  import spire.implicits._

  private implicit val dim: JetDim = JetDim(arity)

  private def jetInputs(xs: Array[Double]): Array[Jet[Double]] = {
    xs.indices.iterator
      .map(i => Jet(xs(i), i))
      .toArray
  }

  private def jet(xs: Array[Double]): Jet[Double] = f(jetInputs(xs))

  override def evalOne(input: Array[Double], n: Int): Double = {
    val out = jet(input)
    if(n == 0)
      out.real
    else
      out.infinitesimal(n - 1)
  }

  override def writeEvalRange(input: Array[Double],
                              first: Int,
                              last: Int,
                              output: Array[Double]): Unit = {
    val out = jet(input)
    var i   = first
    while(i <= last) {
      if(i == 0)
        output(i - first) = out.real
      else
        output(i - first) = out.infinitesimal(i - 1)
      i += 1
    }
  }

}
