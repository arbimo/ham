package ham.parsing


import fastparse._
import JavaWhitespace._
import fastparse.Parsed.{Failure, Success}
import ham.parsing.expr.OperatorClimbing


object Parser {

  val base = OperatorClimbing.base

  private def expr[_: P]: P[AST] = base.expr

  private def function[_: P]: P[Decl] = P(base.ident ~ ("(" ~ base.ident.rep(sep = ",") ~ ")").? ~ "=" ~/ expr ).map {
    case (id, args, body) =>
      def asLambda(args: List[Sym], body: AST): AST = args match {
        case Nil => body
        case h :: t => Lambda(h, asLambda(t, body))
      }
      Decl(id, asLambda(args.getOrElse(Nil).toList, body))
  }

  private def sourceFile[_: P]: P[Seq[Decl]] = Pass ~ function.rep ~ End

  def declarations(source: String): Either[fastparse.Parsed.Failure, List[Decl]] =
    fastparse.parse(source, sourceFile(_)) match {
      case Success(value, index) => Right(value.toList)
      case x: Failure => Left(x)
    }

}
