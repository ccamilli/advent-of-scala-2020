import scala.io.Source
import scala.annotation.tailrec

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

def binarySearch(seq: String, upChar: Char): Int = {
  @tailrec
  def binarySearchAcc(seq: String, upChar: Char, pow: Int, acc:Int): Int = {
    if (seq.length == 0) acc else {
      val mult = if (seq.head == upChar) 1 else 0
      binarySearchAcc(seq.drop(1), upChar, pow*2, acc + mult*pow)
    }
  }
  binarySearchAcc(seq.reverse, upChar, 1, 0)
}

def parseLine(row: String): Int =
  8*binarySearch(row take 7, 'B') + binarySearch(row takeRight 3, 'R')

def solvePart1(filename: String): Int =
  readInputFile(filename).map(x => parseLine(x)).max

def solvePart2(filename: String): Int = {
  val seatIds = readInputFile(filename).map(x => parseLine(x)).toSet
  (seatIds.min to seatIds.max).filter(x => !seatIds.contains(x)).head
}

val filepath = "your_path_here\\inputs\\input_5.txt"

solvePart1(filepath)
solvePart2(filepath)