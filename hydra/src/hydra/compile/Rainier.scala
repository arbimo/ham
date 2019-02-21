package hydra.compile

import com.stripe.rainier.core._
import com.stripe.rainier.compute._
import spire.math.{Jet, JetDim}

object Rainier extends App {

  def compile(params: IndexedSeq[Variable], f: Real): FunN = {
    val grads                                = f.variables.zip(f.gradient).toMap
    val funs: Array[Array[Double] => Double] = Array.ofDim(params.length + 1)
    val compiler                             = Compiler.default
    funs(0) = compiler.compile(params, f)
    for(i <- params.indices) {
      val grad = grads.get(params(i)) match {
        case Some(r) => r
        case None    => Real.zero
      }
      funs(i + 1) = compiler.compile(params, grad)
    }
    new FunNExplicit(params.size, funs)
  }

  val three = Real(3)

  val x = new Variable
  val y = new Variable

  val f = x.max(0) //* (y + 1)

  f.gradient
  f.writeGraph("/tmp/f")
  f.gradient.head.writeGraph("/tmp/df")

  val eval = new Evaluator(Map(x -> 3.0, y -> 4.0))
  println(eval.toDouble(f))
  println(List(x, y))
  println(f.variables)
  println(f)
  f.gradient.foreach(df => {
    println(df)
    println(eval.toDouble(df))
  })

  val fc = Compiler.default.compile(List(x, y), f)
  println(fc.apply(Array(3.0, 4.0)))
  val grads = Compiler.withGradient("fc", f, List(x, y))
  val dfc   = Compiler.default.compile(List(x, y), grads)

  val fn  = compile(Array(x), f)
  val fn2 = new NumericDiff(fc, 1)
  val X   = Array(3.1, 4.0)
  println(fn.eval(X).mkString(", "))
  println(fn2.eval(X).mkString(", "))

}
