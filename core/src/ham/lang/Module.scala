package ham.lang

import cats.implicits._
import ham.expr._
import ham.errors._
import ham.parsing.Decl
import ham.typing.Typer
import ham.utils.Graph

import scala.collection.mutable

class Module(name: String,
             imports: Seq[Import],
             private val expressions: Map[String, ham.expr.Expr]) {
  val id: ModuleID = ModuleID(name)

  def mainFunction: Option[Id] =
    if(expressions.contains("main")) Some(id / "main")
    else None

  def symbolNameToId(name: String): Id = id / name
  lazy val symbols: Set[Id]            = expressions.keySet.map(symbolNameToId)
  lazy val definitions: Map[Id, Expr] = expressions
    .map { case (name, expr) => symbolNameToId(name) -> expr }
  def definition(sym: Id): Attempt[Expr] = {
    if(sym.module != id)
      failure(s"Looking up symbol $sym in module $id")
    else
      definitions.get(sym).toRight(error(s"No symbol $sym in module $id"))
  }

  lazy val qualifiedNames: Map[String, Id]   = symbols.map(id => id.global -> id).toMap
  lazy val unqualifiedNames: Map[String, Id] = symbols.map(id => id.local  -> id).toMap

  lazy val dependencies: Map[Id, Set[Id]] = expressions.map {
    case (k, v) => symbolNameToId(k) -> Expr.symbolOccurences(v)
  }

  /** Order of symbols in the module such that a symbol does not depend on any other appearing later in the order */
  def processingOrder: Attempt[List[Id]] =
    Graph.topologicalOrder(symbols, (id: Id) => dependencies(id).filter(_.module == this.id)) match {
      case Right(order) => Right(order.reverse.toList)
      case Left(Graph.Cycle(nodes)) =>
        Left(Typer.error(s"Cycle in symbols: ${nodes.mkString(", ")}"))
    }

  def typeCheck(typeOfExternal: Id => Attempt[Type]): Attempt[Map[Id, Type]] = {
    // mutable map in which to store the computed types for the current module
    val types = mutable.Map[Id, Type]()

    // function that looks for previously computed type, either in our mutable map, or from the external types
    val knownTypes: Id => Attempt[Type] = id =>
      if(id.module == this.id)
        types.get(id) match {
          case Some(t) => Right(t)
          case None if expressions.contains(id.name) =>
            Left(Typer.error(s"Type of $id is not known yet"))
          case None => Left(Typer.error(s"Unknown symbol $id"))
        } else
        typeOfExternal(id)

    val isFail = processingOrder.flatMap(order => {
      order.traverse(sym => {
        assert(!types.contains(sym))
        for {
          expr <- definition(sym)
          tpe  <- Typer.typeOf(expr, knownTypes)
        } yield {
          types.update(sym, tpe)
          ()
        }
      })
    })

    isFail.map(_ => types.toMap)
  }

}

object Module {

  def parse(moduleName: String,
            source: String,
            parser: String => Attempt[List[Decl]],
            moduleLoader: ModuleLoader): Attempt[Module] = {
    val moduleID = ModuleID(moduleName)
    for {
      decls <- parser(source)
      importedSymsPerModule <- moduleLoader.defaultImports.traverse {
        case Import.Qualified(mod) =>
          moduleLoader.load(mod).map(tm => tm.mod.qualifiedNames)
        case Import.UnQualified(mod) =>
          moduleLoader.load(mod).map(tm => tm.mod.qualifiedNames ++ tm.mod.unqualifiedNames)
      }
      // fold right so that the most recent imports (first in list) override the latest ones
      importedSyms = importedSymsPerModule.foldRight(Map[String, Id]()) {
        case (curr, acc) => acc ++ curr
      }
      // add symbols of the current module to the table, making sure they override previously defined ones
      lookUpTable = importedSyms ++ decls.map(_.name.name).map(name => name -> moduleID / name)
      binds <- decls.traverse {
        case ham.parsing.Decl(name, ast) =>
          ham.expr.Expr
            .fromAST(ast, name => lookUpTable.get(name))
            .map(expr => name.name -> expr)
      }
    } yield new Module(moduleName, moduleLoader.defaultImports, binds.toMap)
  }

}

case class TypedModule(mod: Module, types: Map[Id, Type]) {
  def id: ModuleID = mod.id
}
