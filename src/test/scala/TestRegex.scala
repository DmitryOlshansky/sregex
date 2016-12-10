package sregex

import org.scalatest._

class TestRegex extends FlatSpec with Matchers{
  "Regex " must "work" in {
    val regex = Regex("abc")
    println(regex findPrefixMatch("abc"))
  }
}
