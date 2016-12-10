package sregex

import java.util

private[sregex] class Thread(var pc:Int, n:Int) {
  val matches = Array.ofDim[Int](n)
  var next: Thread = null

  def pushFront(t:Thread):Thread = {
    t.next = this
    t
  }
}

private[sregex] class ThreadList {
  var head:Thread = null
  var tail:Thread = null

  def pushFront(t: Thread): Unit = {
    if(head == null) {
      head = t
      tail = t
      t.next = null
    } else {
      t.next = head
      head = t
    }
  }

  def pushBack(t: Thread): Unit = {
    if(tail == null) {
      head = t
      tail = t
      t.next = null
    } else {
      tail.next = t
      tail = t
      tail.next = null
    }
  }

  def empty: Boolean = head == null

  def clear(): Unit = {
    head = null
    tail = null
  }
}

class Match(source: CharSequence, marks: Array[Int]) {
  def after(i: Int): CharSequence = {
    source.subSequence(marks(2*i+1),source.length())
  }
  def after: CharSequence = {
    source.subSequence(marks((marks.length-1)), source.length())
  }
  def before(i: Int): CharSequence = {
    source.subSequence(0, marks(2*i))
  }
  def before: CharSequence = {
    source.subSequence(0, marks(0))
  }
  def group(i: Int):String = {
    source.subSequence(marks(2*i), marks(2*i+1)).toString()
  }
}

case class Regex(program: Program, charTable: Array[(Thread, Evaluator)=>Unit], noCharTable: Array[(Thread, Evaluator)=>Unit]){
  def findPrefixMatch(source: CharSequence):Option[Match] = {
    val eval = new Evaluator(source, this)
    if (eval.matchInput()) Some(new Match(source, eval.matches))
    else None
  }
}

object Regex {
  def apply(pattern:String): Regex = {
    CodeGen(RegexParser(pattern).get).compile()
  }
}

class Evaluator(input: CharSequence, val regex: Regex) {
  var freelist:Thread = null
  var genCounter:Int = 0
  var index:Int = 0
  var front:Int = 0
  var clist = new ThreadList()
  var nlist = new ThreadList()
  var worklist:Thread = null
  val mergeTable = Array.ofDim[Int](regex.program.hotspots)
  val matches = Array.ofDim[Int](regex.program.nGroups)
  var matched:Boolean = false

  def next(): Boolean = {
    if (input.length() == index) false
    else {
      front = input.charAt(index)
      index += 1
      true
    }
  }

  def matchInput():Boolean = {
    clist.pushFront(threadAt(0))
    while (!clist.empty && next()) {
      genCounter += 1
      var n = clist.head
      while(n != null) {
        regex.charTable(n.pc)(n, this)
        n = n.next
      }
      val tmp = clist
      clist = nlist
      nlist = tmp
      nlist.clear()
    }
    genCounter += 1
    // No char branch
    while(!clist.empty) {
      var n = clist.head
      while(n != null) {
        regex.noCharTable(n.pc)(n, this)
        n = n.next
      }
    }
    matched
  }

  def threadAt(newPc: Int): Thread = {
    if (freelist != null) {
      val t = freelist
      freelist = freelist.next
      t.pc = newPc
      util.Arrays.fill(t.matches, 0)
      t
    } else {
      new Thread(newPc, regex.program.nGroups)
    }
  }

  def forkAt(thread:Thread, newPc: Int): Thread = {
    if (freelist != null) {
      val t = freelist
      freelist = freelist.next
      t.pc = newPc
      Array.copy(thread.matches, 0, t.matches, 0, thread.matches.length)
      t
    } else {
      val t = new Thread(newPc, regex.program.nGroups)
      Array.copy(thread.matches, 0, t.matches, 0, thread.matches.length)
      t
    }
  }

  def recycle(thread:Thread) = {
    thread.next = freelist
    freelist = thread
  }

  def finish(thread: Thread) = {
    Array.copy(thread.matches,0, matches, 0, matches.length)
    matched = true
    clist.clear() // cut off low priority threads
  }
}
