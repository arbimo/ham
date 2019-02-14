package ham.eval

object Functions {

  val Add: Double => Double => Double = a => b => a + b
  val Sub: Double => Double => Double = a => b => a - b
  val Mul: Double => Double => Double = a => b => a * b
  val Div: Double => Double => Double = a => b => a / b

  val Abs: Double => Double  = a => math.abs(a)
  val Sqrt: Double => Double = a => math.sqrt(a)

  val Cos: Double => Double = a => math.cos(a)
  val Sin: Double => Double = a => math.sin(a)
  val Tan: Double => Double = a => math.tan(a)

  val LEQ: Double => Double => Boolean = a => b => a <= b
  val GEQ: Double => Double => Boolean = a => b => a >= b

  private def funUnsafe(name: String): Any = name match {
    case "real.add"  => Add
    case "real.sub"  => Sub
    case "real.mul"  => Mul
    case "real.div"  => Div
    case "real.abs"  => Abs
    case "real.sqrt" => Sqrt
    case "real.cos"  => Cos
    case "real.sin"  => Sin
    case "real.tan"  => Tan
    case "real.leq"  => LEQ
    case "real.geq"  => GEQ
    case "real.PI"   => math.Pi
    case _           => null
  }

  def apply(name: String): Option[Any] = funUnsafe(name) match {
    case null => None
    case f    => Some(f)
  }

}
