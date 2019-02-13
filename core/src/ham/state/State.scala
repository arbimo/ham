package ham.state

import scala.collection.immutable.ListMap

class State(_fields: Array[Field]) {

  def fields: Iterable[Field] = _fields
  def numFields: Int = _fields.length

  def offset(f: Field): Option[Int] = _fields.indexOf(f) match {
        case -1 => None
        case index => Some(index)
      }

  def findField(name: String): Option[Field] = _fields.find(_.name == name)



  def arrayRep: StateRep[Array[Word], this.type] = new StateRep[Array[Word], this.type] {
    override def wordFieldReader(f: Field): Option[Array[Word] => Word] = {
      offset(f).map(index => (s: Array[Word]) => s(index))
    }

    override def default(): Array[Word] = new Array[Word](numFields)

    override def wordFieldUpdater(f: Field): Option[Word => Array[Word] => Array[Word]] =
      offset(f).map(idx => (v: Word) => (s: Array[Word]) => s.updated(idx, v))
  }

}

trait StateRep[V, S <: State] {

  def wordFieldUpdater(f: Field): Option[Word => V => V]

  def wordFieldReader(f: Field): Option[V => Word]
  def fieldReader(f: Field): Option[V => f.A] = {
    wordFieldReader(f).map(reader =>
    s => f.tpe.fromWord(reader(s)))
  }

  def default(): V

  def view(v: V, s: S): ListMap[String, Any] =
    s.fields.foldLeft(ListMap.empty[String,Any]) {
      case (acc, f) =>
        fieldReader(f) match {
          case Some(reader) => acc.updated(f.name, reader(v))
          // todo: we should enforce that field reader is complete (e.g. by tying the field definition to the state)
          case None => sys.error("unexpected")
        }
    }


}

abstract class Compiler[S <: State, E] {

  def usedFields(e: E): Set[Field]

  def compile[V](e: E)(implicit rep: StateRep[V, S]): Option[V => Word]


}