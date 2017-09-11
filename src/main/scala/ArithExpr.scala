sealed trait ArithExpr {
  def eval: Int
}

case class Const(c: Int) extends ArithExpr {
  override def eval: Int = c
}

case class Sum(x: ArithExpr, y: ArithExpr) extends ArithExpr {
  override def eval: Int = x.eval + y.eval
}

case class Product(x: ArithExpr, y: ArithExpr) extends ArithExpr {
  override def eval: Int = x.eval * y.eval
}