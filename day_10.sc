import scala.annotation.tailrec
import scala.io.Source

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

def solvePart1(filename: String): Int = {
  @tailrec
  def solvePart1Acc(sortedList: List[Int], diff1: Int, diff3: Int, previousNum: Int): Int = {
    sortedList match {
      case hd::tl => {
        val diff = hd - previousNum
        if (diff == 1) solvePart1Acc(tl, diff1+1, diff3, hd)
        else if (diff == 3) solvePart1Acc(tl, diff1, diff3+1, hd)
        else solvePart1Acc(tl, diff1, diff3, hd)
      }
      case _ => diff1 * (diff3+1)
    }
  }
  solvePart1Acc(readInputFile(filename).sorted, 0, 0, 0)
}

def solvePart2(sortedArray: Array[Int]): Long = {
    if (sortedArray.length <= 2) if (sortedArray.max - sortedArray.min <= 3) 1 else 0
    else {
      val splitPoint: Int = sortedArray.size / 2
      val splitEl: Int = sortedArray(splitPoint)
      val (left, right) = sortedArray.splitAt(splitPoint)
      solvePart2(left ++ Array(splitEl)) * solvePart2(right) +
        solvePart2(sortedArray.filter(x => x != splitEl))
      }
  }

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_10.txt"

val p1 = solvePart1(filepath)
val p2 = solvePart2(Array(0) ++ readInputFile(filepath).sorted.toArray)