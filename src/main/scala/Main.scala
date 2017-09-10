import core._
import string._
import cats.implicits._

object Main extends App {

  val parseLettersAndExclamationMark: Parser[String, (String, Char)] =
    for {
      xs <- many(satisfy(_.isLetter))
      y  <- char('!')
    } yield (xs.mkString, y)

  val parseInt: Parser[String, Int] = some(satisfy(_.isDigit)).map(_.mkString.toInt)

  println(parseLettersAndExclamationMark.runA("Hello!"))
  println(parseInt.runA("42!"))
}
