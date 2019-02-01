package ham.parsing.expr.impl

import fastparse._
import ham.parsing.WhiteSpaceHandler
import ham.parsing.expr.Operator

abstract class OperatorClimbing[E](
                                    ops: List[Operator], whitespace: WhiteSpaceHandler) {

  implicit val handler = whitespace.whitespace

  require(ops.forall(_.arity == 2))

  def atom[_: P]: P[E]
  def app[_: P]: P[E]
  def expr1[_: P] : P[E] = P(app | atom)
  def expr[_: P]: P[E] = P( opApp )

  def complete[_: P]: P[E] = Pass ~ expr ~ End

  def buildOpApplication(o: Operator, params: List[E]): E

  private def binOpStr[_: P]: P[String] = P(CharsWhileIn("!=/*&|+-^:")).! // TODO
  private def binOp[_: P]: P[Operator] = binOpStr
    .filter(s => ops.exists(_.sym == s))
    .map(s => ops.find(_.sym == s).get)

  def opApp[_: P]: P[E] = P(expr1 ~ (binOp ~/ expr1).rep).map { case (pre, fs) =>
    var remaining = fs
    def climb(minPrec: Int, current: E): E = {
      var result = current
      while(
        remaining.headOption match {
          case None => false
          case Some((op, next)) =>
            val prec: Int = op.precedence
            if(prec < minPrec) false
            else {
              remaining = remaining.tail
              val rhs = climb(prec + 1, next)
              result = buildOpApplication(op, List(result, rhs))
              true
            }
        }) {}
      result
    }
    climb(0, pre)
  }
}