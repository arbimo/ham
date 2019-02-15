package hydra

import spire.algebra._
import spire._
import spire.math._
import spire.syntax.all._
import spire.implicits._

abstract class GenFun { self =>
  def arity: Int
  implicit private def jetDim = JetDim(arity)

  def apply[T: Field: Trig](arr: Array[T]): T

  def spec[T: Field: Trig]: Fun[T] = new Fun[T] {
    override def apply(arr: Array[T]): T = self.apply(arr)
  }

  def jetSpec[T: ClassTag: Field: Trig: Eq: NRoot: Signed]: Fun[Jet[T]] = {
    spec[Jet[T]]
  }

  def jetSpec2[T: ClassTag: Field: Trig: Eq: NRoot: Signed]: Array[T] => Jet[T] = {
    val spec: Fun[Jet[T]] = jetSpec[T]
    (arr: Array[T]) =>
      {
        val jets = new Array[Jet[T]](arr.length)
        for(i <- arr.indices)
          jets(i) = Jet(arr(i), i)
        spec.apply(jets)
      }
  }
}

abstract class Fun[@specialized T: Field: Trig] {

  def apply(arr: Array[T]): T
}

object Jets extends App {

  val xValue = 9.47892774
  val yValue = 0.287740

  val vals = Array[Double](xValue, yValue)

//  spire.math.cos()

  def f[T: Field](a: T, b: T): T = Field[T].plus(Field[T].times(a, a), Field[T].times(a, b))
  def f[T: Field](xs: Array[T]): T =
    Field[T].plus(Field[T].times(xs(0), xs(0)), Field[T].times(xs(0), xs(1)))

  val g = new GenFun {
    override def arity: Int                              = 2
    override def apply[T: Field: Trig](arr: Array[T]): T = f(arr)
//      Field[T].plus(arr(0), Field[T].times(arr(0), arr(1)))
  }
  val gDouble = g.spec[Double]
  val gJet    = g.jetSpec[Double]
  val gJet2   = g.jetSpec2[Double]

  println(gDouble(vals))
  println(gJet2(vals))
//  sys.exit()

//  def f[@specialized(Double) T: Field](x: T, y: T): T = x * x + x * y

  def toJets[T: Field: ClassTag](arr: Array[T]): Array[Jet[T]] = {
    implicit val dim = JetDim(arr.length)
    val jets         = new Array[Jet[T]](arr.length)
    for(i <- arr.indices)
      jets(i) = Jet(arr(i), i)
    jets
  }
  {
    implicit val dim = JetDim(vals.length)
    val x            = Field[Jet[Double]]
    println(f(vals))
    println(f(toJets(vals)))
  }

// The "2" means there should be 2 dual number components.
  implicit val dimension = JetDim(2)
  val x: Jet[Double]     = xValue + Jet.h[Double](0); // Pick the 0th dual number for x.
  val y: Jet[Double]     = yValue + Jet.h[Double](1); // Pick the 1th dual number for y.

  val z: Jet[Double]    = f(x, y)
  val zArr: Jet[Double] = f(Array(x, y))
  println("df/dx = " + z.infinitesimal(0) + ", df/dy = " + z.infinitesimal(1))
  println("df/dx = " + zArr.infinitesimal(0) + ", df/dy = " + zArr.infinitesimal(1))

}
