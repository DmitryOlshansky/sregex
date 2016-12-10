package sregex

import scala.collection.immutable.Set
import scala.util.parsing.combinator._
import scala.util.{Try, Success, Failure}

import com.ibm.icu.text.UnicodeSet

sealed trait Ast
sealed trait Atom extends Ast
case class CharAtom(c:scala.Char) extends Atom
case object AnyAtom extends Atom
case class CharSetAtom(set: UnicodeSet) extends Atom
case class Group(capturing: Boolean, content: Alternative) extends Atom
case class Quantifier(greedy: Boolean, min: Int, max: Int)
case class Piece(atom: Atom, q:  Quantifier) extends Ast
case class Sequence(seq: List[Piece]) extends Ast
case class Alternative(alts: List[Sequence]) extends Ast

case class RegexException(msg: String, cause:Throwable=null) extends Exception(msg, cause)

object RegexParser extends RegexParsers {
	override def skipWhitespace: Boolean =  false
	def character: Parser[CharAtom] = """[^?.{}()\\|*+\[\]]""".r ^^ {
		x => CharAtom(x.charAt(0)) 
	}
	def hexEscape: Parser[CharAtom] = """\\x[0-9a-fA-F]{2}""".r ^^ { 
		x => CharAtom(Integer.parseInt(x.substring(2), 16).toChar)
	}
	def any : Parser[Atom] = "." ^^ { _ => AnyAtom }
	def group: Parser[Atom] = "(" ~ alternative ~ ")" ^^ { x => Group(true, x._1._2) }
	def nonCapturingGroup: Parser[Atom] = "(?:" ~ alternative ~ ")" ^^ { x => Group(false, x._1._2) }
	def charsetString: Parser[String] = "[" ~ rep1(charsetString | "\\[" | """[^\[\]]+""".r) ~ "]" ^^ ( x => x._1._1 + x._1._2.mkString + x._2)
	def charset: Parser[Atom] = charsetString ^^ { x => CharSetAtom(new UnicodeSet(x)) }
	def atom: Parser[Atom] = any | character | charset | hexEscape | nonCapturingGroup | group
	def star: Parser[Quantifier] = "*" ^^ { _ => Quantifier(true, 0, -1) }
	def plus: Parser[Quantifier] = "+" ^^ { _ => Quantifier(true, 1, -1) }
	def nongreedyStar: Parser[Quantifier] = "*?" ^^ { _ => Quantifier(false, 0, -1) }
	def nongreedyPlus: Parser[Quantifier] = "+?" ^^ { _ => Quantifier(false, 1, -1) }
	def number: Parser[Int] = """\d+""".r ^^ { x => Integer.parseInt(x) }
	def counted1: Parser[Quantifier] = "{" ~ number ~ "}" ^^ { 
		x => Quantifier(true, x._1._2, x._1._2)
	}
	def counted2: Parser[Quantifier] = "{" ~ number ~ "," ~ number ~ "}" ^^ {
		x => Quantifier(true, x._1._1._1._2, x._1._2)
	}
	def nongreedyCounted1: Parser[Quantifier] = counted1 ~ "?" ^^ { 
		x => Quantifier(false, x._1.min, x._1.max)
	}
	def nongreedyCounted2: Parser[Quantifier] = counted2 ~ "?" ^^ {
		x => Quantifier(false, x._1.min, x._1.max)
	}
	def question: Parser[Quantifier] = "?" ^^ { _ => Quantifier(true, 0, 1) }
	def noquantifier: Parser[Quantifier] = "" ^^ { x => Quantifier(true, 1, 1) }
	def quantifier: Parser[Quantifier] = nongreedyStar | nongreedyPlus | nongreedyCounted1 | 
		nongreedyCounted2 | question | star | plus | counted1 | counted2 | noquantifier
	def piece: Parser[Piece] = atom ~ quantifier ^^ { x => Piece(x._1, x._2) }
	def sequence: Parser[Sequence] = rep1(piece) ^^ { x=> Sequence(x) }
	def alternative: Parser[Alternative] = sequence ~ rep( "|" ~ sequence) ^^ { 
		x => Alternative(x._1 :: x._2.map(x => x._2))

	}

	def apply(source: String): Try[Alternative] = parseAll(alternative, source) match {
		case Success(result, _) => scala.util.Success(result)
		case failure: NoSuccess => scala.util.Failure(RegexException(failure.msg))
	}
}
