package hydra

import spire.math.Jet

package object optim {

  type R        = Double
  type RefExpr  = Array[Double] => Jet[Double]
  type RMemory  = Array[Double]
  type RWMemory = Array[Double]

}
