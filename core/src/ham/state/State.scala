package ham.state

class State(fields: Array[Field]) {


  def offset(f: Field): Option[Int] = fields.indexOf(f) match {
        case -1 => None
        case index => Some(index)
      }



  def arrayRep: StateRep[Array[Word], this.type] = new StateRep[Array[Word], this.type] {
    override def wordFieldReader(f: Field): Option[Array[Word] => Word] = {
      offset(f).map(index => (s: Array[Word]) => s(index))
    }
  }

}

trait StateRep[V, S] {

  def wordFieldReader(f: Field): Option[V => Word]
  def fieldReader(f: Field): Option[V => f.A] = {
    wordFieldReader(f).map(reader =>
    s => f.tpe.fromWord(reader(s)))
  }


}

abstract class Compiler[S <: State, E] {

  def usedFields(e: E): Set[Field]

  def compile[V](e: E)(implicit rep: StateRep[V, S]): Option[V => Word]


}