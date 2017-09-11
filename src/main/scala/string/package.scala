import core._
import cats.implicits._

package object string {
  type StringParser[T] = Parser[String, T]

  def satisfy(p: Char => Boolean): StringParser[Char] = Parser { input =>
    input.headOption match {
      case Some(c) if p(c) => Some(input.tail, c)
      case _               => None
    }
  }

  def char(c: Char): StringParser[Char] = satisfy(c == _)

  def charNot(c: Char): StringParser[Char] = satisfy(c != _)

  def str(s: String): StringParser[String] = s.toList.map(char).sequence.map(_.mkString)

  def anyOf(s: String): StringParser[Char] = satisfy(s.contains(_))

  def noneOf(s: String): StringParser[Char] = satisfy(!s.contains(_))
}
