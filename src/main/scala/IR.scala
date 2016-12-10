package sregex

import com.ibm.icu.text.UnicodeSet
import scala.collection._

sealed trait IR

case object Any extends IR
case class Character(c: Int) extends IR
case class CharSet(set: UnicodeSet) extends IR
case class Marker(n: Int) extends IR
case class Goto(delta: Int) extends IR
case class LoopStart(greedy:Boolean, size:Int) extends IR
case class LoopEnd(greedy:Boolean, size:Int, hotspot:Int) extends IR
case class Alt(size:Int) extends IR
case class AltEnd(hotspot:Int) extends IR
case object End extends IR

case class Program(ir: Seq[IR],nGroups:Int, hotspots:Int) {
  def print():Unit = {
    for(i <- 0 until ir.size){
      System.out.println(s"$i ${ir(i)}")
    }
  }

  def compile():Regex = {
    def chainEval(evaluator: Evaluator, table: Array[(Thread,Evaluator)=>Unit]) = {
      if(evaluator.worklist != null) {
        val n = evaluator.worklist
        evaluator.worklist = evaluator.worklist.next
        table(n.pc)(n, evaluator)
      }
    }
    val charTable = ir.map({
      case Any =>
          (t:Thread, evaluator: Evaluator) => {
            t.pc += 1
            evaluator.nlist.pushBack(t)
            chainEval(evaluator, evaluator.regex.charTable)
          }
      case Character(c) =>
          (t: Thread, evaluator: Evaluator) => {
            if(evaluator.front == c) {
              t.pc += 1
              evaluator.nlist.pushBack(t)
            }
            else
              evaluator.recycle(t)
            chainEval(evaluator, evaluator.regex.charTable)
          }
      case CharSet(set) =>
          (t: Thread, evaluator: Evaluator) => {
            if(set.contains(evaluator.front)) {
              t.pc += 1
              evaluator.nlist.pushBack(t)
            }
            else
              evaluator.recycle(t)
            chainEval(evaluator, evaluator.regex.charTable)
          }
      case Marker(n) =>
          (t: Thread, evaluator: Evaluator) => {
            t.matches(n) = evaluator.index
            t.pc += 1
            evaluator.regex.charTable(t.pc)(t, evaluator)
          }
      case Goto(delta) =>
          (t: Thread, evaluator: Evaluator) => {
            t.pc += delta
            evaluator.regex.charTable(t.pc)(t, evaluator)
          }
      case LoopStart(_, size) =>
          (t: Thread, evaluator: Evaluator) => {
            t.pc += size
            evaluator.regex.charTable(t.pc)(t, evaluator)
          }
      case LoopEnd(greedy, size, hotspot) =>
          (t: Thread, evaluator: Evaluator) => {
            if(evaluator.mergeTable(hotspot) == evaluator.genCounter) {
              evaluator.recycle(t)
              chainEval(evaluator, evaluator.regex.charTable)
            }
            else {
              evaluator.mergeTable(hotspot) = evaluator.genCounter
              if (greedy) {
                evaluator.worklist = evaluator.worklist.pushFront(evaluator.forkAt(t, t.pc + 1))
                t.pc -= size
              } else {
                evaluator.worklist = evaluator.worklist.pushFront(evaluator.forkAt(t, t.pc - size))
                t.pc += 1
              }
              evaluator.regex.charTable(t.pc)(t, evaluator)
            }
          }
      case Alt(size) =>
        (t: Thread, evaluator: Evaluator) => {
          ir(t.pc + size) match {
            case _:Alt =>
              evaluator.worklist = evaluator.worklist.pushFront(evaluator.forkAt(t, t.pc + size))
              t.pc += 1
              evaluator.regex.charTable(t.pc)(t, evaluator)
            case _:AltEnd =>
              t.pc += 1
              evaluator.regex.charTable(t.pc)(t, evaluator)
            case _ => assert(false)
          }
        }
      case AltEnd(hotspot) =>
        (t:Thread, evaluator: Evaluator) => {
          if (evaluator.mergeTable(hotspot) == evaluator.genCounter){
            evaluator.recycle(t)
            chainEval(evaluator, evaluator.regex.charTable)
          }
          else {
            evaluator.mergeTable(hotspot) = evaluator.genCounter
            t.pc += 1
            evaluator.regex.charTable(t.pc)(t, evaluator)
          }
        }
      case End =>
        (t: Thread, evaluator: Evaluator) => {
          evaluator.finish(t)
        }
    }).toArray
    val noCharTable = ir.map {
      case Any =>
        (t: Thread, evaluator: Evaluator) => {
          evaluator.recycle(t)
          chainEval(evaluator, evaluator.regex.noCharTable)
        }
      case Character(_) =>
        (t: Thread, evaluator: Evaluator) => {
          evaluator.recycle(t)
          chainEval(evaluator, evaluator.regex.noCharTable)
        }
      case CharSet(_) =>
        (t: Thread, evaluator: Evaluator) => {
          evaluator.recycle(t)
          chainEval(evaluator, evaluator.regex.noCharTable)
        }
      case Marker(n) =>
        (t: Thread, evaluator: Evaluator) => {
          t.matches(n) = evaluator.index
          t.pc += 1
          evaluator.regex.noCharTable(t.pc)(t, evaluator)
        }
      case Goto(delta) =>
        (t: Thread, evaluator: Evaluator) => {
          t.pc += delta
          evaluator.regex.noCharTable(t.pc)(t, evaluator)
        }
      case LoopStart(_, size) =>
        (t: Thread, evaluator: Evaluator) => {
          t.pc += size
          evaluator.regex.noCharTable(t.pc)(t, evaluator)
        }
      case LoopEnd(greedy, size, hotspot) =>
        (t: Thread, evaluator: Evaluator) => {
          if(evaluator.mergeTable(hotspot) == evaluator.genCounter) {
            evaluator.recycle(t)
            chainEval(evaluator, evaluator.regex.noCharTable)
          }
          else {
            evaluator.mergeTable(hotspot) = evaluator.genCounter
            if (greedy) {
              evaluator.worklist = evaluator.worklist.pushFront(evaluator.forkAt(t, t.pc + 1))
              t.pc -= size
            } else {
              evaluator.worklist = evaluator.worklist.pushFront(evaluator.forkAt(t, t.pc - size))
              t.pc += 1
            }
            evaluator.regex.noCharTable(t.pc)(t, evaluator)
          }
        }
      case Alt(size) =>
        (t: Thread, evaluator: Evaluator) => {
          ir(t.pc + size) match {
            case _:Alt =>
              evaluator.worklist = evaluator.worklist.pushFront(evaluator.forkAt(t, t.pc + size))
              t.pc += 1
              evaluator.regex.noCharTable(t.pc)(t, evaluator)
            case _:AltEnd =>
              t.pc += 1
              evaluator.regex.noCharTable(t.pc)(t, evaluator)
            case _ => assert(false)
          }
        }
      case AltEnd(hotspot) =>
        (t:Thread, evaluator: Evaluator) => {
          if (evaluator.mergeTable(hotspot) == evaluator.genCounter){
            evaluator.recycle(t)
            chainEval(evaluator, evaluator.regex.noCharTable)
          }
          else {
            evaluator.mergeTable(hotspot) = evaluator.genCounter
            t.pc += 1
            evaluator.regex.noCharTable(t.pc)(t, evaluator)
          }
        }
      case End =>
        (t: Thread, evaluator: Evaluator) => {
          evaluator.finish(t)
        }
    }.toArray
    Regex(this, charTable, noCharTable)
  }
}

object CodeGen {
  def apply(ast: Ast): Program = new CodeGen().compile(ast)
}

class CodeGen {
  var nGroup = 0
  var hotspotCounter = 0

  def compile(ast: Ast): Program = {
    val ir = (gen(ast) ++ Seq(End)).map({
      case LoopEnd(greedy, size, _) =>
        val x = LoopEnd(greedy, size, hotspotCounter)
        hotspotCounter += 1
        x
      case AltEnd(_) =>
        val x = AltEnd(hotspotCounter)
        hotspotCounter += 1
        x
      case ir:IR => ir
    })
    Program(ir, nGroup, hotspotCounter)
  }

  def gen(ast: Ast): Seq[IR] = ast match {
    case AnyAtom => Seq(Any)
    case CharAtom(c) => Seq(Character(c))
    case CharSetAtom(s) => Seq(CharSet(s))
    case Group(capturing, content) => if (capturing) {
      val (start, end) = (nGroup, nGroup + 1)
      nGroup += 2
      Seq(Marker(start)) ++ this.gen(content) ++ Seq(Marker(end))
    } else this.gen (content)
    case Piece(atom, Quantifier(greedy, min, max)) =>
      if (min == max) {
        val a = this.gen(atom)
        Seq.fill(min)(a).flatten
      }else if (min == 0 && max == 1){
        val content = this.gen(atom)
        if(greedy)
          Seq(Alt(content.size+1)) ++ content ++ Seq(Goto(1), Alt(0), AltEnd(0))
        else
          Seq(Alt(1), Goto(content.size+1), Alt(content.size)) ++ content ++ Seq(AltEnd(0))
      }else if(min == 0 && max > 0) {
        val a = this.gen(Piece(atom, Quantifier(greedy, 0, 1)))
        Seq.fill(max)(a).flatten
      } else if(min == 0 && max < 0) {
        val content = this.gen(atom)
        Seq(LoopStart(greedy, content.size)) ++ content ++ Seq(LoopEnd(greedy, content.size, 0))
      }
      else { // split {n,m} into {n,n} and {0, m-n}
        this.gen(Piece(atom, Quantifier(greedy, min, min))) ++ this.gen(Piece(atom, Quantifier(greedy, 0, max - min)))
      }
    case Sequence(pieces) => pieces.flatMap(this.gen(_))
    case Alternative(alts) => {
      val pieces = alts.map(this.gen(_))
      if (pieces.size == 1)
        pieces(0)
      else {
        val totalLength = pieces.map(_.size).sum + 2*pieces.size
        val buf = mutable.Buffer[IR]()
        var lengthSoFar = 0
        for (i <- 0 until pieces.size) {
          if (i != pieces.size - 1)
            buf.append(Alt(pieces(i).size+1))   // Account for goto at the end of alt.
          else
            buf.append(Alt(pieces(i).size))
          buf.appendAll(pieces(i))
          lengthSoFar += 1 + pieces(i).size
          if (i != pieces.size - 1) {
            buf.append(Goto(totalLength - lengthSoFar - 1))
            lengthSoFar += 1
          }
          else {
            buf.append(AltEnd(0))
          }
        }
        buf
      }
    }
  }
}