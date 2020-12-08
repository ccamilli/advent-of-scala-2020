import scala.io.Source
import scala.annotation.tailrec

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

def solvePart1(input: List[String]) = {
  @tailrec
  def solvePart1Acc(input: List[String], totalAcc: Int, seen: Set[Char]): Int = {
    input match {
      case r::rs => {
        if (r == "") solvePart1Acc(rs, totalAcc + seen.size, Set())
        else solvePart1Acc(rs, totalAcc, r.foldLeft(seen)((s, el) => s + el))
      }
      case _ => totalAcc + seen.size
    }
  }
  solvePart1Acc(input, 0, Set())
}

def solvePart2(input: List[String]): Int = {
  @tailrec
  def solvePart2Acc(input: List[String], totalAcc: Int, count: Int, seen: Set[Char]): Int = {
    input match {
      case r::rs => {
        if (r == "") solvePart2Acc(rs, totalAcc + seen.size, 0, Set())
        else {
          if (count == 0)
            solvePart2Acc(rs, totalAcc, count + 1, r.foldLeft(seen)((s, el) => s + el))
          else
            solvePart2Acc(rs, totalAcc, count + 1, r.filter(c => seen.contains(c)).toSet)
        }
      }
      case _ => totalAcc + seen.size
    }
  }
  solvePart2Acc(input, 0, 0, Set())
  }

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_6.txt"

solvePart1(readInputFile(filepath))
solvePart2(readInputFile(filepath))