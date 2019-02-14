package ham.parsing.expr

final case class Operator(sym: String, arity: Int, precedence: Int)

object Operator {

  val defaults = List(
//    Operator("=>", 2, 1), // implication requires right associativity
    //    Operator("xor", 2, 3),
    Operator("||", 2, 4),
    Operator("&&", 2, 5),
    //    Operator("not", 1, 6), // not is unary
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
}
