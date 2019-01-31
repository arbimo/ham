package ham.expr

import ham.model

trait IExpr[E] {

  def makeConstant(v: Any): E
  def makeApplication(f: E, arg: E): E

  def isConstant(e: E): Boolean
  def valueOfConstant(e: E): Any

  def isSym(e: E): Boolean
  def nameOfSym(e: E): String

  def isApplication(e: E): Boolean
  def functionOfApplication(e: E): E
  def argumentOfApplication(e: E): E

}

object IExpr {

  def apply[E](implicit ev: IExpr[E]): IExpr[E] = ev


  object Cst {
    def unapply[E: IExpr](e: E): Option[Any] =
      if(IExpr[E].isConstant(e)) Some(IExpr[E].valueOfConstant(e))
      else None
  }

  object App {
    def unapplySeq[E: IExpr](e: E): Option[List[E]] =
      if(IExpr[E].isApplication(e)) Some(IExpr[E].functionOfApplication(e) :: IExpr[E].argumentOfApplication(e) :: Nil)
      else None
  }

  object Sym {
    def unapply[E: IExpr](e: E): Option[String] =
      if(IExpr[E].isSym(e)) Some(IExpr[E].nameOfSym(e))
      else None
  }

  implicit val instance: IExpr[ham.model.AST] = new IExpr[model.AST] {
    def error: Nothing = sys.error("match")

    override def makeConstant(v: Any): model.AST = v match {
      case d: Double => ham.model.Num(BigDecimal(d))
      case _ => error
    }

    override def makeApplication(f: model.AST, arg: model.AST): model.AST =
      ham.model.Application(f, arg)

    override def isConstant(e: model.AST): Boolean = e match {
      case _: model.Num => true
      case _ => false
    }

    override def valueOfConstant(e: model.AST): Any = e match {
      case model.Num(rep) => rep.toDouble
      case _ => error
    }

    override def isSym(e: model.AST): Boolean = e match {
      case _: model.Sym => true
      case _ => false
    }

    override def nameOfSym(e: model.AST): String = e match {
      case model.Sym(name) => name
      case _ => error
    }

    override def isApplication(e: model.AST): Boolean = e match {
      case _: model.Application => true
      case _ => false
    }

    override def functionOfApplication(e: model.AST): model.AST = e match {
      case model.Application(f, _) => f
      case _ => error
    }

    override def argumentOfApplication(e: model.AST): model.AST = e match {
      case model.Application(_, a) => a
      case _ => error
    }
  }

}