package hydra

import hydra.memory.StateSequence
import spire.math.Jet

package object optim {

  type R        = Double
  type RefExpr  = Array[Double] => Jet[Double]
  type RMemory  = StateSequence
  type RWMemory = StateSequence

}
