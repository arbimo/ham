package ham.errors

import cats.data.NonEmptyList

class Err(val msg: String, val cause: Throwable = null) extends Exception(msg, cause)

class MultipleErr(errors: NonEmptyList[Err]) extends Err(errors.toList.map(_.msg).mkString("\n"))

final case class ParseError(err: fastparse.Parsed.Failure) extends ham.errors.Err(err.toString())
