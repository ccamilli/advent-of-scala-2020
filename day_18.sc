import scala.annotation.tailrec
import scala.io.Source

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

type HierarchyList = List[(Char, Int)]

@tailrec
def getHierarchyListLoop(exprLeft: String, priority: Int, out: HierarchyList): HierarchyList = {
  if (exprLeft == "") out
  else {
    val c = exprLeft.head
    val next = exprLeft drop 1
    if (c == '(') getHierarchyListLoop(next, priority+1, out)
    else
      if (c == ')') getHierarchyListLoop(next, priority-1, out)
      else getHierarchyListLoop(next, priority, out :+ (c, priority))
  }
}

def getHierarchyList(expr: String): HierarchyList = {
  getHierarchyListLoop(expr, 0, List())
}

def eval(expr: List[String]): String = {
  expr match {
    case nm1::op::nm2::r1::r2 => eval(eval(List(nm1, op, nm2))+:r1+:r2)
    case nm1::op::nm2::Nil => {
      if (op == "+") (BigInt(nm1) + BigInt(nm2)).toString
      else (BigInt(nm1) * BigInt(nm2)).toString
    }
    case nm1::_ => nm1
  }
}

def eval2(expr: List[String]): String = {
  expr match {
    case nm1::op::nm2::r1::r2 => {
      if (op == "+") eval2(eval2(List(nm1, op, nm2))+:r1+:r2)
      else eval2(List(nm1,op,eval2(expr.tail.tail)))
      }
    case nm1::op::nm2::Nil => {
      if (op == "+") (BigInt(nm1) + BigInt(nm2)).toString
      else (BigInt(nm1) * BigInt(nm2)).toString
    }
    case nm1::_ => nm1
  }
}

def splitPriorityMaxLoop(hListLeft: HierarchyList, out: HierarchyList,
                         currAcc: (Int, String), maxP: Int, cumulMode: Boolean,
                         evalFunc: List[String] => String): HierarchyList = {
  hListLeft match {
    case hd::tail => {
      val (c, prio) = hd
      if (prio == maxP && cumulMode) {
        val (ix, str) = currAcc
        splitPriorityMaxLoop(tail, out, (ix, str :+ c), maxP, cumulMode=true, evalFunc)
      }
      else
        if (prio == maxP && !cumulMode) {
          splitPriorityMaxLoop(tail, out, (-1, c.toString), maxP, cumulMode=true, evalFunc)
        }
        else
          if (prio != maxP && cumulMode) {
            splitPriorityMaxLoop(tail,
              out ++ getHierarchyListLoop(evalFunc(currAcc._2.split(" ").toList),
                maxP-1, List()) :+ hd, (-1, ""), maxP, cumulMode=false, evalFunc)
          }
          else {
            splitPriorityMaxLoop(tail, out :+ hd, (-1, ""), maxP, cumulMode=false, evalFunc)
          }
    }
    case _ => {
      if (cumulMode) out ++ getHierarchyListLoop(evalFunc(currAcc._2.split(" ").toList),
        maxP-1, List())
      else out
    }
  }
}

def reduceExpr(expr: String, evalFunc: List[String] => String): BigInt = {
  @tailrec
  def reduceExprLoop(pList: HierarchyList, breakP: Int): HierarchyList = {
    val maxP = pList.map(_._2).max
    if (maxP < breakP) pList
    else
      reduceExprLoop(splitPriorityMaxLoop(pList, List(), (-1, ""), maxP,
        false, evalFunc), breakP)
  }
  val originalList = getHierarchyList(expr)
  val minP = originalList.map(_._2).min
  BigInt(reduceExprLoop(originalList, minP).map(x => x._1.toString).reduce(_+_))
}

def solvePart1(puzzleInput: List[String]): BigInt = {
  puzzleInput.map(x => reduceExpr(x, eval)).sum
}

def solvePart2(puzzleInput: List[String]): BigInt = {
  puzzleInput.map(x => reduceExpr(x, eval2)).sum
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_18.txt"
val input = readInputFile(filepath)

solvePart1(input)
solvePart2(input)