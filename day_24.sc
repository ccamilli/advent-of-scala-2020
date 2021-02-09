import scala.io.Source
import scala.annotation.tailrec

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

case class Coord(x: Int, y: Int) {
  def update(instr: String): Coord = {
    if (instr == "ne") Coord(x+1, y+1)
    else if (instr == "se") Coord(x+1, y-1)
    else if (instr == "nw") Coord(x-1, y+1)
    else if (instr == "sw") Coord(x-1, y-1)
    else if (instr == "e") Coord(x+2, y)
    else Coord(x-2, y)
  }

  def getNeighbors: List[Coord] =
    List(Coord(x+1, y+1), Coord(x+1, y-1), Coord(x-1, y+1), Coord(x-1, y-1), Coord(x+2, y), Coord(x-2, y))
}

case class TileGrid(tileMap: Map[Coord, Int]) {
  def updateAt(coord: Coord): TileGrid = {
    val curColor = tileMap.getOrElse(coord, 1)
    TileGrid(tileMap.updated(coord, curColor * -1))
  }

  def countBlack: Int = tileMap.values.count(_ == -1)

  def countBlackNeighbors(coord: Coord) : Int =
    coord.getNeighbors.count(tileMap.getOrElse(_, 1) == -1)

  def updateTile(coord: Coord): (Coord, Int) = {
    val nBlackNeighbors = countBlackNeighbors(coord)
    val curColor = tileMap.getOrElse(coord, 1)
    if (curColor == 1 && nBlackNeighbors == 2) (coord, -1)
    else if (curColor == -1 && (nBlackNeighbors == 0 || nBlackNeighbors > 2)) (coord, 1)
    else (coord, curColor)
  }

  def updateFullGrid: TileGrid = {
    val (minX, maxX) = (tileMap.keys.map(_.x).min, tileMap.keys.map(_.x).max)
    val (minY, maxY) = (tileMap.keys.map(_.y).min, tileMap.keys.map(_.y).max)
    val allCoords = for (x <- (minX-1) to (maxX+1); y <- (minY-1) to (maxY+1) if (x+y)%2 == 0) yield Coord(x, y)
    TileGrid(allCoords.foldLeft(tileMap){
      case (m, coord) => m + updateTile(coord)
    })
  }

  def updateNTimes(n: Int): TileGrid = {
    @tailrec
    def updateLoop(curGrid: TileGrid, n: Int): TileGrid = {
      if (n == 0) curGrid
      else updateLoop(curGrid.updateFullGrid, n-1)
    }
    updateLoop(this, n)
  }
}

def parseInstructions(instr: String): Coord = {
  @tailrec
  def parseInstructionsLoop(instrLeft: String, coord: Coord): Coord = {
    if (instrLeft == "") coord
    else {
      val takeChar =
        if (instrLeft.startsWith("n") || instrLeft.startsWith("s")) 2
        else 1
      parseInstructionsLoop(instrLeft.drop(takeChar), coord.update(instrLeft.take(takeChar)))
    }
  }
  parseInstructionsLoop(instr, Coord(0, 0))
}

def solvePart1(initGrid: TileGrid): Int = initGrid.countBlack

def solvePart2(initGrid: TileGrid): Int = initGrid.updateNTimes(100).countBlack

val filepath = "your_path_here\\inputs\\input_24.txt"

val puzzleInput = readInputFile(filepath)

val initGrid = puzzleInput.foldLeft(TileGrid(Map.empty)){
  case (m, instr) => m.updateAt(parseInstructions(instr))
}

solvePart1(initGrid)
solvePart2(initGrid)