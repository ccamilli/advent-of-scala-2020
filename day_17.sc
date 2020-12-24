import scala.annotation.tailrec
import scala.io.Source


def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

case class GridPosition(x:Int, y:Int, z:Int, w:Int)
case class Grid(cells: Map[GridPosition, Char], fourD: Boolean = false) {
  val (minX, maxX) = (cells.keys.map(_.x).min, cells.keys.map(_.x).max)
  val (minY, maxY) = (cells.keys.map(_.y).min, cells.keys.map(_.y).max)
  val (minZ, maxZ) = (cells.keys.map(_.z).min, cells.keys.map(_.z).max)
  val (minW, maxW) = if (fourD) (cells.keys.map(_.w).min, cells.keys.map(_.w).max) else (1, -1)

  def getNeighbors(pos: GridPosition): List[GridPosition] = {
    val (curX, curY, curZ, curW) = (pos.x, pos.y, pos.z, pos.w)
    val horizontalIx = List(curX - 1, curX, curX + 1)
    val verticalIx = List(curY - 1, curY, curY + 1)
    val zIx = List(curZ - 1, curZ, curZ + 1)
    val wIx = List(curW - 1, curW, curW + 1)
    for (x <- horizontalIx; y <- verticalIx; z <- zIx; w <- wIx
         if (x, y, z, w) != (curX, curY, curZ, curW))
      yield GridPosition(x, y, z, w)
  }

  def updatePos(pos: GridPosition): Char = {
    val curChar = cells.getOrElse(pos, '.')
    val occupiedNeighbors = getNeighbors(pos).count(x => cells.getOrElse(x, '.') == '#')
    if (curChar == '.' && occupiedNeighbors == 3) '#'
    else if (curChar == '#' && !(occupiedNeighbors == 2 || occupiedNeighbors == 3)) '.'
    else curChar
  }

  def simulateRound(): Grid = {
    Grid((for (x <- minX-1 to maxX+1; y <- minY-1 to maxY+1; z <- minZ-1 to maxZ+1; w <- minW-1 to maxW+1)
        yield (GridPosition(x, y, z, w), updatePos(GridPosition(x, y, z, w)))).toMap, fourD)
  }

  def simulateRounds(nRounds: Int): Grid = {
    @tailrec
    def simulateRoundsAcc(curGrid: Grid, roundsLeft: Int): Grid =
      if (roundsLeft == 0) curGrid else simulateRoundsAcc(curGrid.simulateRound(), roundsLeft - 1)
    simulateRoundsAcc(this, nRounds)
  }
}

def createGrid(puzzleInput: List[String], fourD: Boolean = false): Grid = {
  @tailrec
  def parseColAcc(line: String, acc: Int, y: Int, colAcc: Map[GridPosition, Char]): Map[GridPosition, Char] = {
    if (line.isEmpty) colAcc
    else parseColAcc(line.tail, acc+1, y, colAcc + (GridPosition(acc, y, 0, 0) -> line.head))
  }

  @tailrec
  def parseRowsAcc(inputLeft: List[String], y: Int, gridAcc: Map[GridPosition, Char]): Grid = {
    inputLeft match {
      case hd::tl =>{
        parseRowsAcc(tl, y+1, gridAcc ++ parseColAcc(hd, 0, y, Map()))
      }
      case _ => Grid(gridAcc, fourD)
    }
  }
  parseRowsAcc(puzzleInput, 0, Map())
}

def solvePart1(puzzleInput: List[String]): Long = {
  createGrid(puzzleInput).simulateRounds(6).cells.count{
    case (_, v) => (v == '#')
  }
}

def solvePart2(puzzleInput: List[String]): Long = {
  createGrid(puzzleInput, fourD=true).simulateRounds(6).cells.count{
    case (_, v) => (v == '#')
  }
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_17.txt"

solvePart1(readInputFile(filepath))
solvePart2(readInputFile(filepath))