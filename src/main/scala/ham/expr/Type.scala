package ham.expr

import scala.collection.mutable
import scala.reflect.ClassTag

sealed abstract class Type {
  override def toString: String = Type.show(this)
}

object Type {

  val functionSymbol = "->"

  def primitive(name: String): Type = Oper(name, Nil)

  def function(from: Type, to: Type): Type = Oper(functionSymbol, List(from, to))
  def function(from1: Type, from2: Type, to: Type): Type = function(from1, function(from2, to))
  def function(from1: Type, from2: Type, from3: Type, to: Type): Type = function(from1, function(from2, from3, to))

  final class Var() extends Type {
    var instance: Option[Type] = None
  }
  final case class Oper(name: String, args: List[Type]) extends Type


  def show(tpe: Type): String = {
    val view: mutable.Map[Type.Var, String] = new mutable.HashMap()

    def impl(tpe: Type): String =  tpe match {
      case v: Var => v.instance match {
        case Some(i) => impl(i)
        case None =>
          if(!view.contains(v)) {
            val newName = ('α'.toInt + view.size).toChar.toString
            view.update(v, newName)
          }
          view(v)
      }
      case Oper(name, args) => {
        if (args.length == 0)
          name
        else if (args.length == 2)
          "(" + impl(args(0)) + " " + name + " " + impl(args(1)) + ")"
        else
          args.mkString(name + " ", " ", "")
      }
    }
    impl(tpe)
  }



}


