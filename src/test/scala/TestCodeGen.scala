package sregex

import org.scalatest._

class TestCodeGen extends FlatSpec with Matchers{
  "CodeGen" should "generate correct bytecode" in {
    assert(CodeGen(RegexParser("abc").get).ir == List(Character(97), Character(98), Character(99), End))
    CodeGen(RegexParser("a(?:bc)+?").get).print()
  }
}
