package ham.parsing.expr

import ham.parsing._
import fastparse._
import ham.errors.Attempt
import ham.parsing.expr.impl.OperatorClimbing

trait LangParser[AST] {
  def expr[_: P]: P[AST]
  def ident[_: P]: P[String]
  def lit[_: P]: P[String]

}

object LangParser {

  val default = expressionParser()

  def expressionParser(
      operators: List[Operator] = Operator.defaults,
      whitespace: WhiteSpaceHandler = WhiteSpaceHandler.Java,
      literals: Parser[String] = Literals.reals,
      identifiers: Parser[String] = Identifers.alpha
  ): LangParser[AST] = new ExprParser(operators, whitespace, literals, identifiers)

  private class ExprParser(
      operators: List[Operator],
      whitespace: WhiteSpaceHandler,
      literals: Parser[String],
      identifiers: Parser[String]
  ) extends OperatorClimbing[AST](operators, whitespace)
      with LangParser[AST] {

    override def ident[_: P]: P[String] = identifiers.apply

    def lit[_: P]: P[String] = literals.apply

    def literal[_: P]: P[Lit] = P(lit.map(Lit(_)))

    def symbol[_: P]: P[Sym] = P(Index ~ identifiers.apply ~ Index).map {
      case (si, str, _) => Sym(str)
    }

    override def atom[_: P]: P[AST] = P(symbol | literal)

    override def app[_: P]: P[AST] = P(symbol ~ "(" ~/ expr.rep(sep = ",") ~ ")").map {
      case (f, args) => Application(f, args.toList)
    }

    override def buildOpApplication(o: Operator, params: List[AST]): AST =
      Application(Sym(o.sym), params)

  }

}
