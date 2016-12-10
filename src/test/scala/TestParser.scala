package sregex

import org.scalatest._

import scala.util.{Failure, Success}

class TestParser extends FlatSpec with Matchers {
	"Regex parser" should "parse" in {
		RegexParser("abc") shouldBe a [Success[_]]
    RegexParser("(") shouldBe a [Failure[_]]
    RegexParser("a++") shouldBe a [Failure[_]]
    RegexParser(".|(a+|c*)") shouldBe a [Success[_]]
    RegexParser("a*") shouldBe a [Success[_]]
    RegexParser("a{2,3}?b{3}c{1,5}(a){6}?") shouldBe a [Success[_]]
    RegexParser("""[[[:Arabic:]\[\p{ASCII}]]""") shouldBe a [Success[_]]
	}
}