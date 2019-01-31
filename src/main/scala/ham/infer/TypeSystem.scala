package ham.infer

object TypeSystem {
  var _nextVariableName = 'α'
	def nextUniqueName = {
		val result = _nextVariableName
		_nextVariableName = (_nextVariableName.toInt + 1).toChar
		result.toString
	}
	var _nextVariableId = 0
	def newVariable: Variable = {
		val result = _nextVariableId
		_nextVariableId += 1
		new Variable(result)
	}

  sealed abstract class Type
	class Variable(id: Int) extends Type {
		var instance: Option[Type] = None
		lazy val name = nextUniqueName
	}
	case class Oper(name: String, args: Seq[Type]) extends Type

  def Function(from: Type, to: Type) = Oper("→", Array(from, to))
	val Integer = Oper("int", Seq())
	val Bool = Oper("bool", Seq())

  def string(t: Type): String = t match {
		case v: Variable => v.instance match {
			case Some(i) => string(i)
			case None => v.name
		}
		case Oper(name, args) => {
			if (args.length == 0)
				name
			else if (args.length == 2)
				"("+string(args(0))+" "+name+" "+string(args(1))+")"
			else
				args.mkString(name + " ", " ", "")
		}
	}

}

import TypeSystem._

sealed abstract class ExprF[F]
case class Lbd[F](param: String, body: F) extends ExprF[F]
case class App[F](fun: F, arg: F) extends ExprF[F]
case class Sym[F](name: String) extends ExprF[F]
case class Lit[F](value: Any, tpe: Type) extends ExprF[F]



import scala.collection.mutable

case class AST[X](root: X, coalg: X => ExprF[X])
case class Env[K](map: Map[K, Type]) {
  def contains(k: K) = map.contains(k)
  def apply(k: K): Option[Type] = map.get(k)
  def +(bind: (K, Type)) = Env(map + bind)
}

class TypeChecker[X](ast: AST[X], env: Env[String]) {
  type E = Env[String]
  def parseError(msg: String) = sys.error(msg)
  def typeError(msg: String) = sys.error(msg)

  val types = mutable.Map[X, Type]()

  def analyse(ast: X, env: E): Type = analyse(ast, env, Set.empty)
	def analyse(node: X, env: E, nongen: Set[Variable]): Type = ast.coalg(node) match {
    case Lit(_, tpe) => tpe
    case Sym(name) => getType(name, nongen)
    case App(fun, arg) =>
      val funType = analyse(fun, env, nongen)
      val argType = analyse(arg, env, nongen)
      val resutType = newVariable
      unify(Function(argType, resutType), funType)
      resutType
    case Lbd(param, body) =>
      val argType = newVariable
      val resultType = analyse(body, env + (param -> argType), nongen + argType)
      Function(argType, resultType)
	}

  def unify(t1: Type, t2: Type) {
		val type1 = prune(t1)
		val type2 = prune(t2)
		(type1, type2) match {
			case (a: Variable, b) => if (a != b) {
				if (occursintype(a, b))
					typeError("recursive unification")
				a.instance = Some(b)
			}
			case (a: Oper, b: Variable) => unify(b, a)
			case (a: Oper, b: Oper) => {
				if (a.name != b.name ||
					a.args.length != b.args.length) typeError("Type mismatch: "+string(a)+"≠"+string(b))

				for(i <- 0 until a.args.length)
					unify(a.args(i), b.args(i))
			}
		}
	}

  def getType(name: String, nongen: Set[Variable]): Type = {
    env(name) match {
      case Some(tpe) => fresh(tpe, nongen)
      case None =>
        parseError("Undefined symbol "+name)
    }
  }

  // Note: must be called with v 'pre-pruned'
	def isgeneric(v: Variable, nongen: Set[Variable]) = !(occursin(v, nongen))

  def fresh(t: Type, nongen: Set[Variable]) = {
		import scala.collection.mutable
		val mappings = new mutable.HashMap[Variable, Variable]
		def freshrec(tp: Type): Type = {
			prune(tp) match {
				case v: Variable =>
					if (isgeneric(v, nongen))
						mappings.getOrElseUpdate(v, newVariable)
					else
						v

				case Oper(name, args) =>
					Oper(name, args.map(freshrec(_)))
			}
		}

		freshrec(t)
	}

  // Returns the currently defining instance of t.
	// As a side effect, collapses the list of type instances.
	def prune(t: Type): Type = t match {
		case v: Variable if v.instance.isDefined => {
			val inst = prune(v.instance.get)
			v.instance = Some(inst)
			inst
		}
		case _ => t
	}

  // Note: must be called with v 'pre-pruned'
	def occursintype(v: Variable, type2: Type): Boolean = {
		prune(type2) match {
			case `v` => true
			case Oper(name, args) => occursin(v, args)
			case _ => false
		}
	}

	def occursin(t: Variable, list: Iterable[Type]) =
		list exists (t2 => occursintype(t, t2))

}