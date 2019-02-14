package ham.lang

import ham.expr.ModuleID

sealed abstract class Import

object Import {
  final case class Qualified(mod: ModuleID)   extends Import
  final case class UnQualified(mod: ModuleID) extends Import
}
