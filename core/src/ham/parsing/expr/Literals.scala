package ham.parsing.expr

import fastparse._

object Literals {

  object integers extends Parser[String] {
    override def apply[_: P]: P[String] = P(CharsWhileIn("0-9").!)
  }


}
