//package ham.hybrid

//import fastparse.Parsed.{Failure, Success}
//import ham.eval.Eval
//import ham.expr.Type
//import ham.expr.Type.Oper
//
//case class Constant[E](name: String, tpe: Type, value: E)
//case class Fluent(name: String, tpe: Type)
//case class Control(name: String, tpe: Type)
//case class Dynamics[E](l: List[Dynamic[E]])
//case class Dynamic[E](fluent: String, value: E)
//case class Constraints[E](l: List[E])
//
//case class HamModel[E](
//              constants: List[Constant[E]],
//              fluents: List[Fluent],
//              controls: List[Control],
//              dynamics: List[Dynamic[E]],
//              constraints: List[E]
//              ) {
//
//  def map[B](f: E => B): HamModel[B] = {
//    HamModel(
//      constants.map(c => Constant(c.name, c.tpe, f(c.value))),
//      fluents,
//      controls,
//      dynamics.map(d => Dynamic(d.fluent, f(d.value))),
//      constraints.map(f)
//    )
//  }
//
//  def definitions: Map[String, Type] = {
//    (constants.map(c => (c.name -> c.tpe)) ++
//      fluents.map(f => (f.name -> f.tpe)) ++
//      controls.map(c => (c.name -> c.tpe))).toMap
//  }
//
//  type State = Array[Double]
//  val stateSize = fluents.size + controls.size
//  def genState(): State = Array.fill(stateSize)(0)
//
//  def constantValue(name: String): Option[E] = constants.find(_.name == name).map(_.value)
//  def indexInState(str: String): Option[Int] =
//    fluents.find(_.name == str) match {
//      case Some(fl) => Some(fluents.indexOf(fl))
//      case None =>
//        controls.find(_.name == str) match {
//          case Some(ctl) => Some(fluents.size + controls.indexOf(ctl))
//          case None => None
//
//        }
//    }
//
//
//  def compile(e: E)(implicit ev: ham.expr.IExpr[E]): State => Any = {
//    def ofSym(str: String): Option[Either[State => Any, E]] = {
//      constantValue(str) match {
//        case Some(e) =>
//          Some(Right(e))
//        case None => indexInState(str) match {
//          case Some(i) =>
//            Some(Left((s: State) => s(i)))
//          case None => ham.eval.Functions(str) match {
//            case Some(f) =>
//              Some(Left((_: State) => f))
//            case None =>
//              None
//          }
//        }
//      }
//    }
//    Eval.evaluator[State, E](e, ofSym)
//  }
//}
//
//object HamModel extends App {
//
//  import fastparse._
//  import JavaWhitespace._
//  val base = OperatorClimbing.base
//  import base._
//
//  def makeType(name: String): Type = Oper(name, Nil)
//
//  def constantParser[_: P]: P[Constant[AST]] = P("constant" ~/ ident ~/ ":" ~ ident ~ "=" ~ expr ~ ";").map {
//    case (name, tpe, value) => Constant(name.name, makeType(tpe.name), value)
//  }
//  def fluentParser[_: P]: P[Fluent] =     P("fluent" ~/ ident ~ ":" ~ ident ~ ";").map {
//    case (id, tpe) => Fluent(id.name, makeType(tpe.name))
//  }
//  def controlParser[_: P]: P[Control] = P("control" ~/ ident ~ ":" ~ ident ~ ";").map {
//    case (id, tpe) => Control(id.name, makeType(tpe.name))
//  }
//  def dynamicParser[_: P]: P[Dynamic[AST]] = P("dot" ~ "(" ~ ident ~ ")" ~ "=" ~ expr ~ ";").map {
//    case (id, value) => Dynamic(id.name, value)
//  }
//  def dynamicsParser[_: P] :P[Dynamics[AST]] = P("dynamics" ~ "{" ~ dynamicParser.rep ~ "}").map(l => Dynamics(l.toList))
//
//  def constraintsParser[_: P]: P[Constraints[AST]] = P("subject_to" ~/ "{" ~ (expr ~ ";").rep ~ "}").map(l => Constraints(l.toList))
//
//  def parseAll[_: P]: P[Seq[Any]] = Pass ~ P(constantParser | fluentParser | controlParser | dynamicsParser | constraintsParser).rep ~ End
//
//
//
//
//
//  def parse(str: String): HamModel[AST] = {
//    fastparse.parse(str, parseAll(_)) match {
//      case Success(value, _) =>
//        val empty = HamModel[AST](Nil, Nil, Nil, Nil, Nil)
//        value.foldLeft(empty) { case (mod, x) => x match {
//          case x: Constant[AST] => mod.copy(constants = x :: mod.constants)
//          case x: Fluent => mod.copy(fluents = x :: mod.fluents)
//          case x: Control => mod.copy(controls = x :: mod.controls)
//          case x: Dynamics[AST] =>
//            mod.copy(dynamics = mod.dynamics ++ x.l)
//          case x: Constraints[AST] =>
//            mod.copy(constraints = mod.constraints ++ x.l)
//        }
//        }
//
//      case fail @ Failure(label, index, extra) =>
//        sys.error(fail.toString())
//    }
//  }
//
//  val res = parse(
//    """
//constant L: Real = 10;
//constant v_max: Real = 10;
//constant v_min: Real = 3;
//constant steer_max: Real = PI / 4;
//
//fluent x : Real;
//fluent y : Real;
//fluent theta: Real;
//control v : Real;
//control s : Real;
//
//dynamics {
//  dot(x) = v * cos(theta);
//  dot(y) = v * sin(theta);
//  dot(theta) = v * tan(s) / L;
//}
//subject_to {
//  v <= v_max;
//  v >= v_min;
//  abs(theta) <= steer_max;
//  PI;
//}
//
//    """.stripMargin)
//
//  println(res)
//  val defs = res.definitions
////  val typed = res.map(e => TypeChecker.typeOf(e, defs))
////  println(typed)
//
//  import ham.expr.IExpr._
//
//  val s = res.genState()
//  res.dynamics.foreach { d =>
//    println(d.fluent)
//    println(d.value)
//    val f = res.compile(d.value)
//    println(f(s))
//  }
//
//  res.constraints.foreach { c =>
//    println(c)
//    val f = res.compile(c)
//    println(f(s))
//  }
//
//}
