package ham.parsing

sealed trait AST

final case class Lit(rep: String) extends AST {
  override def toString: String = rep
}
final case class Sym(name: String) extends AST {
  override def toString: String = name
}
final case class Application(f: AST, arg: AST) extends AST {
  override def toString: String = s"($f $arg)"
}
object Application {
  def apply(f: AST, args: List[AST]): AST = args match {
    case Nil    => f
    case h :: t => apply(Application(f, h), t)
  }
}
final case class Lambda(arg: Sym, body: AST) extends AST

final case class Decl(name: Sym, expr: AST)
