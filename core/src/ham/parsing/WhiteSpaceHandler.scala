package ham.parsing

import fastparse.ParsingRun

trait WhiteSpaceHandler {

  implicit def whitespace: ParsingRun[_] => ParsingRun[Unit]

}

object WhiteSpaceHandler {

  val Java = new WhiteSpaceHandler {
    override implicit def whitespace: ParsingRun[_] => ParsingRun[Unit] =
      fastparse.JavaWhitespace.whitespace
  }
}
