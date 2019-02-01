package ham.parsing.expr

import fastparse.{P, CharsWhileIn}

object Identifers {

  object alpha extends Parser[String] {
    def apply[_: P]: P[String] = P( CharsWhileIn("a-zA-Z_")).!
  }




}
