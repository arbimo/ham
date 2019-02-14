package ham.parsing.expr

import fastparse.{CharsWhileIn, P}

object Identifers {

  object alpha extends Parser[String] {
    def apply[_: P]: P[String] = P(CharsWhileIn("a-zA-Z_")).!
  }

}
