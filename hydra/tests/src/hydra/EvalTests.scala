package hydra

import cats.implicits._

import ham.errors._

import ham.expr.Expr
import ham.prelude.Prelude
import ham.state.{Field, State, Word}
import ham.typing.Typer

object EvalTests extends App {

  val source = """constant Pi: Real = 3.14159;

fluent x: Real;
fluent y: Real;


subject_to {
  x - 10;
  y - 5;
  x - cos(PI);
}"""

  val prelude = Prelude.typedPrelude

  val x = for {
    mod <- Parser.parse(source)
    syms = prelude.mod.unqualifiedNames ++ mod.symbols
    modExpr <- mod.mapErr(x => Expr.fromAST(x, id => syms.get(id)))

  } yield {

    val fields   = mod.fluents.map(f => Field.real(f.name))
    val s        = new State(fields.toArray)
    val adap     = s.arrayRep
    val s0       = adap.default()
    val xUpdater = adap.wordFieldUpdater(fields.head).get
    val s1       = xUpdater(1)(s0)
    println(adap.view(s0, s))
    println(adap.view(s1, s))

    val types = prelude.types ++ modExpr.types
    val csts  = modExpr.constants.map(c => modExpr.moduleID / c.name -> c.value).toMap
    for(c <- modExpr.constraints) {
      val tpe = Typer.typeOf(c, id => types.get(id).toAttempt(ham.errors.error(s"unknown ID $id")))
      println(c)
      println(tpe)

      val ev = Parser.evaluator[Array[Word], Expr](
        c,
        id => {
          csts.get(id).orElse(prelude.mod.definition(id).toOption) match {
            case Some(e) =>
              Some(Right(e))

            case None =>
              s.findField(id.local)
                .flatMap(f => adap.fieldReader(f))
                .map(extractor => Left(extractor))
          }
        },
        builtInName => ham.eval.Functions(builtInName)
      )
      println(ev(s0))
      println(ev(s1))
    }
    modExpr
  }

  println(x)

}
