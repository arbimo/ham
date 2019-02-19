package hydra.optim

import cats.kernel.Semigroup
import spire.math.{Jet, JetDim}
import spire.implicits._

final class Grad(val vars: Array[Int], val derivs: Array[Double]) {
  require(vars.length == derivs.length)
  def length: Int = vars.length
}

case class DiffFun(bridge: Bridge, impl: DiffFunImpl) {

  def eval(xs: Array[Double]): Double = {
    impl.eval(bridge.adapt(xs))
  }

  def diff(xs: Array[Double]): Grad = {
    val diff = impl.diff(bridge.adapt(xs))
    assert(diff.length == bridge.outArity)
    new Grad(bridge.backward, diff)
  }

}

class DiffFunImpl(val arity: Int, f: Array[Jet[Double]] => Jet[Double]) {

  private implicit val dim: JetDim = JetDim(arity)

  private def jetInputs(xs: Array[Double]): Array[Jet[Double]] = {
    xs.indices.iterator
      .map(       i => Jet(xs(i), i))
      .toArray
  }

  def jet(xs: Array[Double]): Jet[Double]    = f(jetInputs(xs))
  def eval(xs: Array[Double]): Double        = jet(xs).real
  def diff(xs: Array[Double]): Array[Double] = jet(xs).infinitesimal

}

trait RealFun[Dom] {

  def eval(dom: Dom): Double
  def diff(dom: Dom): Jet[Double]

}

/**
  * paramMap(i) = j implies
  *   if 0 <= j < outArity: the (i + offset) th parameter of becomes the jth parameter
  *   if -1: the (i+ offset) parameter is not used
  *   otherwise this is an error
  */
final case class Bridge(paramMap: Array[Int], offset: Int, _outArity: Int) {
  def inArity: Int  = paramMap.length
  def outArity: Int = _outArity

  val backward: Array[Int] = {
    val tmp = new Array[Int](_outArity)
    paramMap.indices.foreach(i => {
      val j = paramMap(i)
      if(j != -1)
        tmp(j) = offset + i
    })
    tmp
  }

  def writeAdapted(in: Array[R], out: Array[R]): Unit = {
    assert(in.length - offset >= inArity)
    assert(out.length >= outArity)
    var i = 0
    while(i < inArity) {
      val j = paramMap(i)
      if(j != -1) {
        out(j) = in(offset + i)
      }
      i += 1
    }
  }

  def adapt(in: Array[Double]): Array[Double] = {
    val out = new Array[Double](outArity)
    writeAdapted(in, out)
    out
  }

  def combine(o: Bridge): Bridge = {
    assert(outArity >= o.inArity)
    assert(o.offset == 0, "Not implemented yet")
    val m = (0 until inArity).map(i => o.paramMap(paramMap(i))).toArray
    new Bridge(m, offset, o.outArity)
  }

  def shiftRight(n: Int): Bridge = this.copy(offset = offset + n)
}

object Bridge {

  def identity(arity: Int): Bridge =
    new Bridge(paramMap = (0 until arity).toArray, offset = 0, arity)
  def mapped(map: Map[Int, Int]): Bridge = {
    val inArity  = map.keys.max
    val outArity = map.values.max
    assert(map.values.toSet.size == outArity)
    val arrMap = (0 until inArity).map(i => map.getOrElse(i, -1)).toArray
    new Bridge(arrMap, offset = 0, outArity)
  }

}
