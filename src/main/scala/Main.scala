import core._
import string._
import cats.implicits._

object Main extends App {
  val parseInt: StringParser[Int]                = some(satisfy(_.isDigit)).map(_.mkString.toInt)
  val parseConst: StringParser[ArithExpr]        = parseInt.map(Const)
  lazy val parseExpr: StringParser[ArithExpr]    = parseSummand.sepBy(char('+')).map(_.reduceLeft(Sum))
  lazy val parseSummand: StringParser[ArithExpr] = parseMultiplicand.sepBy(char('*')).map(_.reduceLeft(Product))
  val parseMultiplicand: StringParser[ArithExpr] = parseConst <|> (char('(') *> parseExpr <* char(')'))

  print("Enter an expression consisting of numbers, +, *, and parenthesis: ")
  println(parseExpr.runA(scala.io.StdIn.readLine()).map(_.eval))
}
