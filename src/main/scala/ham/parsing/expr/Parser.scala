package ham.parsing.expr

import ham.parsing._

import fastparse._
import JavaWhitespace._

object SourceParser {

  val base = OperatorClimbing.base

  def expr[_: P]: P[AST] = base.expr

  def function[_: P]: P[Decl] = P("function" ~/ base.ident ~ "(" ~ base.ident.rep(sep = ",") ~ ")" ~ "{" ~/ expr ~ "}" ).map {
    case (id, args, body) =>
      def asLambda(args: List[Sym], body: AST): AST = args match {
        case Nil => body
        case h :: t => Lambda(h, asLambda(t, body))
      }
      Decl(id, asLambda(args.toList, body))
  }

  def constant[_: P]: P[Decl] = P(base.ident ~ "=" ~ expr).map { case (id, expr) => Decl(id, expr)}

  def declaration[_: P]: P[Decl] = P(function | constant)

}

final case class Operator(sym: String, arity: Int, precedence: Int)


abstract class OperatorClimbing[E](
                                    ops: List[Operator]) {
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

object OperatorClimbing {

  val ops = List(
    Operator("=>", 2, 1),
    //    Operator("xor", 2, 3),
    Operator("||", 2, 4),
    Operator("&&", 2, 5),
    //    Operator("not", 1, 6),
    Operator("==", 2, 7),
    Operator("!=", 2, 7),
    Operator("<", 2, 7),
    Operator(">", 2, 7),
    Operator("<=", 2, 7),
    Operator(">=", 2, 7),
    Operator("+", 2, 13),
    Operator("-", 2, 13),
    Operator("*", 2, 14),
    Operator("/", 2, 14),
  )

  object base extends OperatorClimbing[AST](ops) {
    def number[_: P]: P[Num] = P( CharsWhileIn("0-9").!.map(x => Num(BigDecimal(x)) ))
    def identStr[_: P]: P[String] = P( CharsWhileIn("a-zA-Z_")).!
    def ident[_: P]: P[Sym] = P( Index ~ identStr ~ Index).map {
      case (si, str, _) => Sym(str)
    }
    override def atom[_: P]: P[AST] = P(ident | number)
    override def app[_: P]: P[AST] = P( ident ~ "(" ~/ expr.rep(sep = ",") ~ ")").map {
      case (f, args) => Application(f, args.toList)
    }

    override def buildOpApplication(o: Operator, params: List[AST]): AST = Application(Sym(o.sym), params)

  }

}