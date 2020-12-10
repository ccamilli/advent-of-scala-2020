import scala.annotation.tailrec
import scala.io.Source

def readInputFile(filename: String): List[Long] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret.map(x => x.toLong)
}

def twoSum(vals: List[Long], target: Long): Option[(Long, Long)] = {
  @tailrec
  def twoSumAcc(vals_left: List[Long], target: Long, seen: Set[Long]): Option[(Long, Long)] = {
    vals_left match {
      case v::vs => if (seen.contains(target - v)) Some((v, target - v)) else twoSumAcc(vs, target, seen ++ Set(v))
      case _ => None
    }
  }
  twoSumAcc(vals, target, Set.empty)
}

def solvePart1(input: List[Long]): Long = {
  @tailrec
  def solvePart1Acc(inputLeft: List[Long], searchSpace: List[Long]): Long = {
    if (searchSpace.length < 25) solvePart1Acc(inputLeft.tail, searchSpace appended inputLeft.head)
    else (
      twoSum(searchSpace, inputLeft.head) match {
        case Some(_) => solvePart1Acc(inputLeft.tail, searchSpace.tail appended inputLeft.head)
        case None => inputLeft.head
      }
    )
  }
  solvePart1Acc(input, List())
}

def solvePart2(input: List[Long], target: Long): Long = {
  @tailrec
  def solvePart2Acc(inputLeft: List[Long], inputToSum: List[Long], target: Long, cumul: Long, seen: Set[Long]): Long = {
    if (cumul > target) solvePart2Acc(inputLeft.tail, inputLeft.tail, target, 0, Set())
    else
      if (cumul == target) seen.min + seen.max
      else solvePart2Acc(inputLeft, inputToSum.tail, target, cumul + inputToSum.head, seen + inputToSum.head)
  }
  solvePart2Acc(input, input, target, 0, Set())
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_9.txt"

val p1 = solvePart1(readInputFile(filepath))
val p2 = solvePart2(readInputFile(filepath), p1)
