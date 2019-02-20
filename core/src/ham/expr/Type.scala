package ham.expr

import ham.expr
import ham.expr.Type.Oper

import scala.collection.mutable
import scala.reflect.ClassTag

sealed abstract class Type {
  override def toString: String = Type.show(this)

  lazy val arity: Int = this match {
    case Oper(Type.functionSymbol, List(_, b)) => 1 + b.arity
    case _                                     => 0
  }
}

object Type {

  val functionSymbol = "->"

  def primitive(name: String): Type = Oper(name, Nil)

  def function(from: Type, to: Type): Type               = Oper(functionSymbol, List(from, to))
  def function(from1: Type, from2: Type, to: Type): Type = function(from1, function(from2, to))
  def function(from1: Type, from2: Type, from3: Type, to: Type): Type =
    function(from1, function(from2, from3, to))
  def forall(f: Type => Type): Type = f(new Type.Var)

  final class Var()                                     extends Type
  final case class Oper(name: String, args: List[Type]) extends Type

  def show(tpe: Type): String = {
    val view: mutable.Map[Type.Var, String] = new mutable.HashMap()

    def impl(tpe: Type): String = tpe match {
      case v: Var =>
        if(!view.contains(v)) {
          val newName = ('α'.toInt + view.size).toChar.toString
          view.update(v, newName)
        }
        view(v)
      case Oper(name, args) => {
        if(args.length == 0)
          name
        else if(args.length == 2)
          "(" + impl(args(0)) + " " + name + " " + impl(args(1)) + ")"
        else
          args.mkString(name + " ", " ", "")
      }
    }
    impl(tpe)
  }

  def equivalent(lhs: Type, rhs: Type): Boolean = {
    val xs        = mutable.Map[Type.Var, Int]()
    val ys        = mutable.Map[Type.Var, Int]()
    var next: Int = 0
    def unifiable(lhs: Type, rhs: Type): Boolean = (lhs, rhs) match {
      case (x: Var, y: Var) =>
        if(xs.contains(x) && ys.contains(y))
          xs(x) == ys(y)
        else if(xs.contains(x) || ys.contains(y))
          false // one already seen but not the other
        else {
          next += 1
          xs(x) = next
          ys(y) = next
          true
        }
      case (Oper(xname, xargs), Oper(yname, yargs)) =>
        if(xname != yname)
          false
        else if(xargs.length != yargs.length)
          false
        else {
          xargs.zip(yargs).forall { case (x, y) => unifiable(x, y) }
        }
      case _ => false

    }
    unifiable(lhs, rhs)
  }

}
