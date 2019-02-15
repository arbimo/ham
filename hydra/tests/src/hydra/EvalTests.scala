package hydra

import cats.implicits._
import spire.implicits._
import ham.errors._
import ham.expr.{Expr, Id}
import ham.prelude.Prelude
import ham.state.{State, StateField, Word}
import ham.typing.Typer
import spire.math._

object EvalTests extends App {

  val source = """constant Pi: Real = 3.14159;

fluent x: Real;
fluent y: Real;


subject_to {
  x + y - 10;
  y - 5;
  //x - y;
  //x - cos(PI);
  //cos(x);
  //sin(x);
}"""

  val prelude = Prelude.typedPrelude

  def autoDiffBuiltIns(name: String)(implicit dim: JetDim): Option[Any] = Option(
    name match {
      case "real.sub" =>
        (x: Jet[Double]) => (y: Jet[Double]) =>
          x - y
      case "real.cos" =>
        (x: Jet[Double]) =>
          x.cos()

      case "real.sin" =>
        (x: Jet[Double]) =>
          x.sin()
      case "real.PI" => Jet[Double](math.Pi)
      case _         => null
    }
  )

  val x = for {
    mod <- Parser.parse(source)
    syms = prelude.mod.unqualifiedNames ++ mod.symbols
    modExpr <- mod.mapErr(x => Expr.fromAST(x, id => syms.get(id)))

  } yield {

    val fields     = mod.fluents.map(f => StateField.real(f.name))
    val stateShape = new State(fields.toArray)
    val adap       = stateShape.arrayRep
    val s0         = adap.default()
    val xUpdater   = adap.wordFieldUpdater(fields.head).get
    val s1         = xUpdater(1)(s0)
    println(adap.view(s0, stateShape))
    println(adap.view(s1, stateShape))

    val types = prelude.types ++ modExpr.types
    val csts  = modExpr.constants.map(c => modExpr.moduleID / c.name -> c.value).toMap

    val defs: Id => Option[Expr] = id => csts.get(id).orElse(prelude.mod.definition(id).toOption)

    for(c <- modExpr.constraints) {
      val tpe = Typer.typeOf(c, id => types.get(id).toAttempt(ham.errors.error(s"unknown ID $id")))

      val ev = Compiler.evaluator(c, stateShape, defs)

      val differentiator = Compiler.differentiator(c, stateShape, defs)

      println()
      println(s"$c  : $tpe")
      for(s <- List(s0, s1)) {
        println(adap.view(s, stateShape))
        println("  " + ev(s))
        println("  " + differentiator(s))

      }
    }
    val errors = modExpr.constraints.map(c => Compiler.differentiator(c, stateShape, defs))
    val sg     = optimize(s0, errors, 3)
    println(adap.view(sg, stateShape))
    modExpr
  }

  def optimize(s: Array[Double],
               constraints: Seq[Array[Double] => Jet[Double]],
               iters: Int): Array[Double] = {
    import spire.implicits._
    import spire.syntax.all._

    if(iters == 0)
      return s

    val update = Array.fill[Double](s.length)(0)
    println()
    for(c <- constraints) {
      val jet = c(s)
      val up  = jet.infinitesimal.map(g => -g * jet.real)
      println(jet)
      println(up.mkString("[", ", ", "]"))
      for(i <- update.indices) {
        update(i) = update(i) + up(i)
      }
    }
    for(i <- s.indices) {
      s(i) = s(i) + update(i)
    }
    println(update.mkString(", "))
    return optimize(s, constraints, iters - 1)

  }

  println(x)

}
