import scala.io.Source
import scala.annotation.tailrec

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

def binarySearch(seq: String, upChar: Char, downChar: Char): Int = {
  @tailrec
  def binarySearchAcc(seq: String, upChar: Char, downChar: Char, pow: Int, acc:Int): Int = {
    if (seq.length == 0) acc else {
      val mult = if (seq.head == upChar) 1 else 0
      binarySearchAcc(seq.drop(1), upChar, downChar, pow*2, acc + mult*pow)
    }
  }
  binarySearchAcc(seq.reverse, upChar, downChar, 1, 0)
}

def parseLine(row: String): Int = {
  val rowInfo = row take 7
  val columnInfo = row takeRight 3
  8*binarySearch(rowInfo, 'B', 'F') + binarySearch(columnInfo, 'R', 'L')
}

def solvePart1(filename: String) = {
  readInputFile(filename).map(x => parseLine(x)).max
}

def solvePart2(filename: String): Int = {
  val seatIds = readInputFile(filename).map(x => parseLine(x)).toSet
  val min = seatIds.min
  val max = seatIds.max
  (min to max).filter(x => !seatIds.contains(x)).head
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_5.txt"

solvePart1(filepath)
solvePart2(filepath)