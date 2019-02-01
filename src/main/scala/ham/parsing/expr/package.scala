package ham.parsing

import fastparse._

package object expr {

  trait Parser[A] {
    def apply[_: P]: P[A]

    import MultiLineWhitespace._
    /* Convenience method to parse an entire input, ignore whitespace and new lines */
    def complete[_: P]: P[A] = Pass ~ apply ~ End
  }


}
