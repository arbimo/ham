package ham.state

import scala.collection.immutable.ListMap

class State(_fields: Array[StateField]) {

  def fields: Iterable[StateField] = _fields
  def numFields: Int               = _fields.length

  def offset(f: StateField): Option[Int] = _fields.indexOf(f) match {
    case -1    => None
    case index => Some(index)
  }
  def offset(name: String): Option[Int] = findField(name).flatMap(offset)

  def findField(name: String): Option[StateField] = _fields.find(_.name == name)

  def arrayRep: StateRep[Array[Word], this.type] = new StateRep[Array[Word], this.type] {
    override def wordFieldReader(f: StateField): Option[Array[Word] => Word] = {
      offset(f).map(index => (s: Array[Word]) => s(index))
    }

    override def default(): Array[Word] = new Array[Word](numFields)

    override def wordFieldUpdater(f: StateField): Option[Word => Array[Word] => Array[Word]] =
      offset(f).map(idx => (v: Word) => (s: Array[Word]) => s.updated(idx, v))
  }

}

trait StateRep[V, S <: State] {

  def wordFieldUpdater(f: StateField): Option[Word => V => V]

  def wordFieldReader(f: StateField): Option[V => Word]
  def fieldReader(f: StateField): Option[V => f.A] = {
    wordFieldReader(f).map(reader => s => f.tpe.fromWord(reader(s)))
  }

  def default(): V

  def view(v: V, s: S): ListMap[String, Any] =
    s.fields.foldLeft(ListMap.empty[String, Any]) {
      case (acc, f) =>
        fieldReader(f) match {
          case Some(reader) => acc.updated(f.name, reader(v))
          // todo: we should enforce that field reader is complete (e.g. by tying the field definition to the state)
          case None => sys.error("unexpected")
        }
    }

}

abstract class Compiler[S <: State, E] {

  def usedFields(e: E): Set[StateField]

  def compile[V](e: E)(implicit rep: StateRep[V, S]): Option[V => Word]

}
