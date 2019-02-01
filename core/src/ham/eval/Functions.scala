package ham.eval

object Functions {

  val Add: Double => Double => Double = a => b => a + b
  val Mul: Double => Double => Double = a => b => a * b
  val Div: Double => Double => Double = a => b => a / b

  val Abs: Double => Double = a => math.abs(a)

  val Cos: Double => Double = a => math.cos(a)
  val Sin: Double => Double = a => math.sin(a)
  val Tan: Double => Double = a => math.tan(a)

  val LEQ: Double => Double => Boolean = a => b => a <= b
  val GEQ: Double => Double => Boolean = a => b => a >= b


  private def funUnsafe(name: String): Any = name match {
    case "real.add" => Add
    case "*" => Mul
    case "/" => Div
    case "abs" => Abs
    case "cos" => Cos
    case "sin" => Sin
    case "tan" => Tan
    case "<=" => LEQ
    case ">=" => GEQ
    case "PI" => math.Pi
    case _ => null
  }

  def apply(name: String): Option[Any] = funUnsafe(name) match {
    case null => None
    case f => Some(f)
  }

}
