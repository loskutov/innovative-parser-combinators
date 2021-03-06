import cats.implicits._

package object core {
  import cats.data.StateT
  import cats.data.StateT.pure

  type Parser[T, V] = StateT[Option, T, V]

  object Parser {
    def apply[T, V](f: T => Option[(T, V)]): Parser[T, V] = StateT(f)
  }

  def many[T, V](parser: Parser[T, V]): Parser[T, Seq[V]] = some(parser) <+> pure(Seq.empty)

  def some[T, V](parser: Parser[T, V]): Parser[T, Seq[V]] =
    for {
      x  <- parser
      xs <- many(parser)
    } yield x +: xs

  implicit class ParserOps[T, V](p: Parser[T, V]) {
    def sepBy(sep: Parser[T, _]): Parser[T, Seq[V]] =
      for {
        head <- p
        rest <- many(sep *> p)
      } yield head +: rest
    def <|>(q: => Parser[T, V]): Parser[T, V] = Parser { input =>
      p.run(input) orElse q.run(input)
    }
  }
}
