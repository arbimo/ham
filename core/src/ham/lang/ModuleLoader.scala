package ham.lang

import cats.implicits._

import ham.errors._
import ham.expr._
import ham.parsing.Decl

import scala.collection.mutable

trait ModuleLoader {
  def defaultImports: List[Import]
  def load(id: ModuleID): Attempt[TypedModule]

  def environment: Env
}

trait Env {
  def typeOf(id: Id): Attempt[Type]
  def definitionOf(id: Id): Attempt[Expr]
}

class SimpleLoader(predef: List[Module], override val defaultImports: List[Import])
    extends ModuleLoader
    with Env {

  private val loaded = mutable.Map[ModuleID, TypedModule]()

  override def environment: Env = this

  override def typeOf(id: Id): Attempt[Type] = loaded.get(id.module) match {
    case Some(TypedModule(mod, types)) =>
      types.get(id) match {
        case Some(tpe) => Succ(tpe)
        case None =>
          assert(!mod.symbols.contains(id),
                 "Invariant failure: a symbol appears in the module but not in the types table")
          failure(s"Id $id does not appear in module ${id.module}")
      }
    case None => failure(s"Module ${id.module} is not loaded")
  }

  override def definitionOf(id: Id): Attempt[Expr] = loaded.get(id.module) match {
    case Some(TypedModule(mod, _)) =>
      mod.definitions.get(id) match {
        case Some(tpe) => Succ(tpe)
        case None =>
          failure(s"Id $id does not appear in module ${id.module}")
      }
    case None => failure(s"Module ${id.module} is not loaded")
  }

  def typeCheck(module: Module): Attempt[TypedModule] = {
    module.typeCheck(typeOf).map(types => TypedModule(module, types))
  }

  def loadFromSource(id: ModuleID,
                     source: String,
                     parser: String => Attempt[List[Decl]]): Attempt[TypedModule] = {
    for {
      mod   <- Module.parse(id.name, source, parser, this)
      typed <- typeCheck(mod)
      _     <- load(typed)
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
