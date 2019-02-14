package ham.parsing

import fastparse._

package object expr {

  trait Parser[A] {
    def apply[_: P]: P[A]

    import MultiLineWhitespace._
    /* Convenience method to parse an entire input, ignore whitespace and new lines.
     * No notion of comments since that would force passing it as a parameter or forcing an arbitrary one. */
    def complete[_: P]: P[A] = Pass ~ apply ~ End
  }

}
