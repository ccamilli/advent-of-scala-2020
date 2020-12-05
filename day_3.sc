import scala.io.Source
import scala.annotation.tailrec

def readInputFile(filename: String): Array[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toArray
  testTxtSource.close()
  ret
}

def solvePart1(puzzleInput: Array[String], slopeX: Int = 3, slopeY: Int = 1): Int = {
  @tailrec
  def solveAcc(puzzleInput: Array[String], pX: Int, pY: Int, acc: Int): Int = {
    val newX = (pX + slopeX) % puzzleInput(0).length
    val newY = (pY + slopeY)
    if (newY >= puzzleInput.length) acc else {
      val lookup: Char = puzzleInput(newY)(newX)
      val newAcc = if (lookup == '#') acc + 1 else acc
      solveAcc(puzzleInput, newX, newY, newAcc)
    }
    }
  solveAcc(puzzleInput, 0, 0, 0)
}

def solvePart2(puzzleInput: Array[String], slopes: List[(Int, Int)]): Long = {
  slopes.map{case (x, y) => solvePart1(puzzleInput, x, y).toLong}.product
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_3.txt"

val puzzleInput = readInputFile(filepath)

val listSlopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))

solvePart1(puzzleInput)
solvePart2(puzzleInput, listSlopes)