package ham.script

import cats.implicits._
import ham.errors._
import ham.expr._
import ham.typing.Typer
import ham.utils.Graph

import scala.collection.mutable

sealed abstract class Import
object Import {
  final case class Qualified(mod: ModuleID) extends Import
  final case class UnQualified(mod: ModuleID) extends Import
}

class Module(name: String, imports: Seq[Import], private val expressions: Map[String, ham.expr.Expr]) {
  val id: ModuleID = ModuleID(name)

  def symbolNameToId(name: String): Id = id / name
  lazy val symbols: Set[Id] = expressions.keySet.map(symbolNameToId)
  lazy val definitions: Map[Id, Expr] = expressions
    .map { case (name, expr) => symbolNameToId(name) -> expr }
  def definition(sym: Id): Attempt[Expr] = {
    if(sym.module != id)
      failure(s"Looking up symbol $sym in module $id")
    else
      definitions.get(sym).toRight(error(s"No symbol $sym in module $id"))
  }

  lazy val qualifiedNames: Map[String, Id] = symbols.map(id => id.global -> id).toMap
  lazy val unqualifiedNames: Map[String, Id] = symbols.map(id => id.local -> id).toMap

  lazy val dependencies: Map[Id, Set[Id]] = expressions.map {
    case (k, v) => symbolNameToId(k) -> Expr.symbolOccurences(v)
  }

  def topoOrder: Attempt[List[Id]] = Graph.topologicalOrder(
    symbols,
    (id: Id) => dependencies(id).filter(_.module == this.id)) match {
    case Right(order) => Right(order.toList)
    case Left(Graph.Cycle(nodes)) => Left(Typer.error(s"Cycle in symbols: ${nodes.mkString(", ")}"))
  }

  def typeCheck(typeOfExternal: Id => Attempt[Type]): Attempt[Map[Id, Type]] = {
    // mutable map in which to store the computed types for the current module
    val types = mutable.Map[Id, Type]()

    // function that looks for previously computed type, either in our mutable map, or from the external types
    val knownTypes: Id => Attempt[Type] = id =>
      if(id.module == this.id)
        types.get(id) match {
          case Some(t) => Right(t)
          case None if expressions.contains(id.name) => Left(Typer.error(s"Type of $id is not known yet"))
          case None => Left(Typer.error(s"Unknown symbol $id"))
        }
      else
        typeOfExternal(id)

    val isFail = topoOrder.flatMap(order => {
      order.traverse(sym => {
        assert(!types.contains(sym))
        for {
          expr <- definition(sym)
          tpe <- Typer.typeOf(expr, knownTypes)
        } yield {
          types.update(sym, tpe)
          ()
        }
      })
    })

    isFail.map(_ => types.toMap)
  }

}


final case class ParseError(err: fastparse.Parsed.Failure) extends ham.errors.Err(err.toString())


object Module {


  def parse(
             moduleName: String,
             source: String,
             moduleLoader: ModuleLoader): Attempt[Module] = {
    val moduleID = ModuleID(moduleName)
    for {
      decls <- Parser.declarations(source).leftMap(ParseError)
      importedSymsPerModule <-
        moduleLoader.defaultImports.traverse {
          case Import.Qualified(mod) =>
            moduleLoader.load(mod).map(tm => tm.mod.qualifiedNames)
          case Import.UnQualified(mod) =>
            moduleLoader.load(mod).map(tm => tm.mod.qualifiedNames ++ tm.mod.unqualifiedNames)
        }
      // fold right so that the most recent imports (first in list) override the latest ones
      importedSyms = importedSymsPerModule.foldRight(Map[String,Id]()){ case (curr, acc) => acc ++ curr }
      // add symbols of the current module to the table, making sure they override previously defined ones
      lookUpTable = importedSyms ++ decls.map(_.name.name).map(name => name -> moduleID / name)
      binds <- decls.traverse {
        case ham.model.Decl(name, ast) =>
          ham.expr.Expr.fromAST(ast, name => lookUpTable.get(name))
          .map(expr => name.name -> expr)
      }
    } yield new Module(moduleName, moduleLoader.defaultImports, binds.toMap)
  }

}

case class TypedModule(mod: Module, types: Map[Id, Type]) {
  def id: ModuleID = mod.id
}

trait ModuleLoader {
  def defaultImports: List[Import]
  def load(id: ModuleID): Attempt[TypedModule]

  def environment: Env
}

trait Env {
  def typeOf(id: Id): Attempt[Type]
  def definitionOf(id: Id): Attempt[Expr]
}

class SimpleLoader(predef: List[Module], override val defaultImports: List[Import]) extends ModuleLoader with Env {

  private val loaded = mutable.Map[ModuleID, TypedModule]()

  override def environment: Env = this

  override def typeOf(id: Id): Attempt[Type] = loaded.get(id.module) match {
    case Some(TypedModule(mod, types)) => types.get(id) match {
      case Some(tpe) => Right(tpe)
      case None =>
        assert(!mod.symbols.contains(id), "Invariant failure: a symbol appears in the module but not in the types table")
        failure(s"Id $id does not appear in module ${id.module}")
    }
    case None => failure(s"Module ${id.module} is not loaded")
  }

  override def definitionOf(id: Id): Attempt[Expr] = loaded.get(id.module) match {
    case Some(TypedModule(mod, _)) => mod.definitions.get(id) match {
      case Some(tpe) => Right(tpe)
      case None =>
        failure(s"Id $id does not appear in module ${id.module}")
    }
    case None => failure(s"Module ${id.module} is not loaded")
  }

  def typeCheck(module: Module): Attempt[TypedModule] = {
    module.typeCheck(typeOf).map(types => TypedModule(module, types))
  }

  def loadFromSource(id: ModuleID, source: String): Attempt[TypedModule] = {
    for {
      mod <- Module.parse(id.name, source, this)
      typed <- typeCheck(mod)
      _ <- load(typed)
    } yield typed
  }

  private def load(m: TypedModule): Attempt[Unit] = {
    if(loaded.contains(m.id))
      failure(s"Module ${m.id} is aleady loaded")
    else {
      loaded.update(m.id, m)
      success(())
    }
  }

  override def load(id: ModuleID): Attempt[TypedModule] = {
    loaded.get(id) match {
      case Some(x) =>
        success(x)
      case None =>
        // try loading from builtin modules
        val result =
          predef.find(_.id == id) match {
            case Some(module) =>
              typeCheck(module)
            case None =>
              failure(s"Unknown module $id")
          }
        result.map(m => { load(m); m })
    }

  }
}

//class HamLoader() {
//
//  val modules = mutable.ArrayBuffer[Module]()
//  val expressions = mutable.Map[Id, Expr]()
//  val dependencies = mutable.Map[Id, Set[Id]]()
//  val types = mutable.Map[Id, Type]()
//
////  val predefSymbols: Map[String, Id] = predef.keySet.map(id => id.local -> id) ++ predef.keySet.map(id => id.global => id)
//
//  def importedSymbols(imports: List[Import]): Attempt[Map[String, Id]] = imports match {
//    case Nil => Right(Map())
//    case first :: tail =>
//        val currentEither = first match {
//          case Import.Qualified(mod)  => modules.find(_.id == mod) match {
//            case Some(m) => Right(m.symbols.map(id => id.global -> id).toMap)
//            case None => failure(s"Unknown module $mod")
//          }
//          case Import.UnQualified(mod) => modules.find(_.id == mod) match {
//            case Some(m) =>
//              Right(m.symbols.map(id => id.global -> id).toMap ++
//                m.symbols.map(id => id.local -> id).toMap)
//            case None => failure(s"Unknown module $mod")
//          }
//        }
//      for {
//        current <- currentEither
//        next <- importedSymbols(tail)
//      } yield current ++ next
//
//    }
//
//
//  def loadModule(module: Module): Either[String, Unit] = {
//    assert(!modules.contains(module))
//    modules += module
//    for((id, expr) <- module.definitions) {
//      assert(!expressions.contains(id))
//      expressions.update(id, expr)
//      dependencies.update(id, Expr.symbolOccurences(expr))
//    }
//
//
//    Graph.topologicalOrder(
//      module.symbols,
//      (id: Id) => dependencies(id).filter(_.module == module.id)) match {
//      case Left(Graph.Cycle(nodes)) => Left(s"Circular dependency in nodes: $nodes")
//      case Right(order) =>
//        val knownTypes: Id => Attempt[Type] = id => types.get(id) match {
//          case Some(t) => Right(t)
//          case None if expressions.contains(id) => failure(s"Type of $id is not known yet")
//          case None => failure(s"Unknown symbol $id")
//        }
//
//        for(sym <- order) {
//          assert(!types.contains(sym))
//          val expr = expressions(sym)
//          Typer.typeOf(expr, knownTypes) match {
//            case Left(err) => return Left(err.msg)
//            case Right(tpe) => types.update(sym, tpe)
//          }
//        }
//        Right(())
//    }
//  }
//
//  def loadFromSource(moduleName: String, source: String, imports: List[Import]) : Either[Any, Unit] = {
//
//    for {
//      mod <- Module.parse(moduleName, source, imports, importedSymbols)
//      _ <- loadModule(mod)
//    } yield ()
//
////      case Validated.Valid(mod) =>
////        loadModule(mod) match {
////          case Right(()) => println("--------------- OK ----------------")
////          case Left(error) =>
////            System.err.println(error)
////        }
////
////      case Validated.Invalid(errs) =>
////        errs.toList.foreach(System.err.println)
////    }
//  }
//
//
//
//}
