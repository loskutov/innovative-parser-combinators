import core._
import string._

import cats.implicits._
import org.scalatest._

import scala.Function._
import scala.util.Random

class testEverything extends FlatSpec with Matchers with OptionValues with LoneElement {
  "char" should "accept the corresponding character" in {
    char('x').run("xyz").value should be("yz", 'x')
  }
  it should "not accept wrong characters" in {
    char('x').run("foo") shouldBe empty
  }

  "satisfy" should "accept everything with const true predicate" in {
    val str = Random.alphanumeric.take(10).mkString
    many(satisfy(const(true))).map(_.mkString).run(str).value should be ("", str)
  }

  "sepBy" should "work with single occurrence without separator" in {
    char('x').sepBy(char(',')).runA("x").value.loneElement shouldBe 'x'
  }

  "sepBy" should "work with a simple example" in {
    some(satisfy(_.isDigit)).map(_.mkString.toInt).sepBy(char(',')).runA("123,456,789").value shouldBe Seq(
      123,
      456,
      789
    )
  }

  "many" should "accept an empty string" in {
    many(char('A')).runA("").value shouldBe none
  }

  "many" should "accept a repeated character" in {
    val repeated = "AAAAAA"
    many(char('A')).map(_.mkString).run(repeated).value should be ("", repeated)
  }

  "some" should "accept a repeated character" in {
    val repeated = "AAAAAA"
    some(char('A')).map(_.mkString).run(repeated).value should be ("", repeated)
  }

  "some" should "decline an empty string" in {
    some(char('A')).run("") shouldBe none
  }
}
