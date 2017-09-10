import core._

package object string {
  def satisfy(p: Char => Boolean): Parser[String, Char] = Parser { input =>
    input.headOption match {
      case Some(c) if p(c) => Some(input.tail, c)
      case _               => None
    }
  }

  def char(c: Char): Parser[String, Char] = satisfy(c == _)
}
