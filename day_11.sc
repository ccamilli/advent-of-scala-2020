import scala.annotation.tailrec
import scala.io.Source

case class GridPosition(x: Int, y: Int)

case class Grid(seats: Map[GridPosition, Char]) {
  val height = seats.keys.map(_.y).max + 1
  val width = seats.keys.map(_.x).max + 1

  def getNeighbors(pos: GridPosition): List[GridPosition] = {
    val curX = pos.x
    val curY = pos.y
    val horizontalIx = List(curX - 1, curX, curX + 1)
    val verticalIx = List(curY - 1, curY, curY + 1)
    for (x <- horizontalIx; y <- verticalIx
         if this.seats.contains(GridPosition(x, y)) && (x, y) != (curX, curY))
      yield GridPosition(x, y)
  }

  def firstNonEmptySeat(pos: GridPosition, direction: (Int, Int)): Char = {
    val (newX, newY) = (pos.x + direction._1, pos.y + direction._2)
    if (newX < 0 || newY < 0 || newX >= width || newY >= height) '.'
    else {
      val newPos = GridPosition(newX, newY)
      if (seats(newPos) != '.') seats(newPos) else firstNonEmptySeat(newPos, direction)
    }
  }

  def updatePos(pos: GridPosition): Char = {
    val curChar = seats(pos)
    val occupiedNeighbors = getNeighbors(pos).count(x => seats(x) == '#')
    if (curChar == 'L' && occupiedNeighbors == 0) '#'
    else if (curChar == '#' && occupiedNeighbors >= 4) 'L'
    else curChar
  }

  def updatePos2(pos: GridPosition): Char = {
    val curChar = seats(pos)
    val directions = List((0, -1), (0, 1), (-1, 0), (1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1))
    val occupiedNeighbors = directions.map(x => firstNonEmptySeat(pos, x)).count(x => x == '#')
    if (curChar == 'L' && occupiedNeighbors == 0) '#'
    else if (curChar == '#' && occupiedNeighbors >= 5) 'L'
    else curChar
  }

  def simulateRounds(nRounds: Int): Grid = {
    def simulateRoundsAcc(curGrid: Grid, roundsLeft: Int): Grid =
      if (roundsLeft == 0) curGrid else simulateRoundsAcc(curGrid.updateRound(), roundsLeft - 1)
    simulateRoundsAcc(this, nRounds)
  }

  def updateRound(): Grid = {
    val newMap = Map.empty[GridPosition, Char]
    Grid(seats.keys.foldLeft(newMap)((map, x) => map + (x -> updatePos(x))))
  }

  def updateRound2(): Grid = {
    val newMap = Map.empty[GridPosition, Char]
    Grid(seats.keys.foldLeft(newMap)((map, x) => map + (x -> updatePos2(x))))
  }

  def countOccupied() : Int =
    seats.values.count(x => x == '#')

  def prettyPrint(): Unit = {
    val width = seats.keys.map(x => x.x).max
    val height = seats.keys.map(x => x.y).max
    val array = Array.fill(height+1, width+1){'.'}
    val test = seats.foldLeft(array){
      case (newArray, (gridPos, char)) =>
      newArray.updated(gridPos.y, newArray(gridPos.y).updated(gridPos.x, char))}
    test.foreach(a =>
    {print(a.mkString(""))
      print('\n')}
    )
  }
}

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

def createGrid(puzzleInput: List[String]): Grid = {
  @tailrec
  def parseColAcc(line: String, acc: Int, y: Int, colAcc: Map[GridPosition, Char]): Map[GridPosition, Char] = {
    if (line.isEmpty) colAcc
    else parseColAcc(line.tail, acc+1, y, colAcc + (GridPosition(acc, y) -> line.head))
  }

  @tailrec
  def parseRowsAcc(inputLeft: List[String], y: Int, gridAcc: Map[GridPosition, Char]): Grid = {
    inputLeft match {
      case hd::tl =>{
        parseRowsAcc(tl, y+1, gridAcc ++ parseColAcc(hd, 0, y, Map()))
      }
      case _ => Grid(gridAcc)
    }
  }
  parseRowsAcc(puzzleInput, 0, Map())
}

@tailrec
def solvePart1(grid: Grid): Int = {
  val newGrid = grid.updateRound()
  if (newGrid == grid) newGrid.countOccupied() else solvePart1(newGrid)
}

@tailrec
def solvePart2(grid: Grid): Int = {
  val newGrid = grid.updateRound2()
  if (newGrid == grid) newGrid.countOccupied() else solvePart2(newGrid)
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_11.txt"

val grid = createGrid(readInputFile(filepath))
solvePart1(grid)
solvePart2(grid)
