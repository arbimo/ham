package ham.state

abstract class StateField {
  type A
  val name: String
  val tpe: PrimitiveType[A]
}
object StateField {
  def real(_name: String): StateField = new StateField {
    override type A = Double
    override val name: String          = _name
    override val tpe: PrimitiveType[A] = PrimitiveType.Real
  }
}

abstract class PrimitiveType[X] {
  final type A = X

  def fromWord(a: Word): A
  def toWord(a: A): Word

}
object PrimitiveType {

  object Real extends PrimitiveType[Double] {
    override def fromWord(a: Word): A = a
    override def toWord(a: A): Word   = a
  }

  object Int extends PrimitiveType[Long] {
    override def fromWord(a: Word): A = java.lang.Double.doubleToRawLongBits(a)
    override def toWord(a: A): Word   = java.lang.Double.longBitsToDouble(a)
  }

}
