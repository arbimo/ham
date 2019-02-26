package hydra.teb
import algebra.Order
import cats.collections.Discrete

import cats.implicits._

class DiscreteTime(val i: Int) extends AnyVal {
  def succ: DiscreteTime = new DiscreteTime(i + 1)
  def pred: DiscreteTime = new DiscreteTime(i - 1)

  def to(last: DiscreteTime): Iterator[DiscreteTime] =
    (i to last.i).iterator.map(new DiscreteTime(_))
}
object DiscreteTime {
  def apply(i: Int): DiscreteTime = new DiscreteTime(i)

  implicit val discrete: Discrete[DiscreteTime] = new Discrete[DiscreteTime] {
    override def succ(x: DiscreteTime): DiscreteTime = x.succ
    override def pred(x: DiscreteTime): DiscreteTime = x.pred
  }
  implicit val order: Order[DiscreteTime] =
    (x: DiscreteTime, y: DiscreteTime) => Order[Int].compare(x.i, y.i)
}

trait HybridTime[D, C] {

  def timesOf(dt: D): cats.collections.Range[C]
  def discreteTimes(t: C): Set[D]
  def discreteTimes: cats.collections.Range[D]
  def continuousTimes: cats.collections.Range[C]
}
