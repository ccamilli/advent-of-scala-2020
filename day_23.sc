import scala.io.Source
import scala.math.floorMod
import scala.annotation.tailrec


def readInputFile(filename: String): Array[Int] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret.head.map(x => x.toString.toInt).toArray
}

@tailrec
def simulateCupsEfficiently(cupStats: Map[Int, Int], curEl: Int,
                            movesLeft: Int, modulo: Int): Map[Int, Int] = {
  if (movesLeft == 0) cupStats
  else {
    val r1 = cupStats(curEl)
    val r2 = cupStats(r1)
    val r3 = cupStats(r2)
    val newEl = cupStats(r3)

    val elems = Set(r1, r2, r3)

    val targetLabel = {
      if (!elems.contains(floorMod(curEl - 2, modulo) + 1)) floorMod(curEl - 2, modulo) + 1
      else
        if (!elems.contains(floorMod(curEl - 3, modulo) + 1)) floorMod(curEl - 3, modulo) + 1
        else
          if (!elems.contains(floorMod(curEl - 4, modulo) + 1)) floorMod(curEl - 4, modulo) + 1
          else floorMod(curEl - 5, modulo) + 1
    }

    val lbRight = cupStats(targetLabel)

    val listUpdates = List((curEl, newEl), (targetLabel, r1), (r3, lbRight))

    simulateCupsEfficiently(listUpdates.foldLeft(cupStats)((x, el) => x + el),
      newEl, movesLeft - 1, modulo)
  }
}

def initializeCupStats(puzzleInput: Array[Int]): Map[Int, Int] = {
  val mod = puzzleInput.length
  puzzleInput.zipWithIndex.map{
    case (el, ix) => (el, puzzleInput(floorMod(ix + 1, mod)))
  }.toMap
}

@tailrec
def prettyPrint(map: Map[Int, Int], curVal: Int, cumul: String): String = {
  if (curVal == 1) cumul
  else prettyPrint(map, map(curVal), cumul + curVal)
}

def solvePart1(puzzleInput: Array[Int]): String = {
  val initialMap = initializeCupStats(puzzleInput)
  val finalMap = simulateCupsEfficiently(initialMap, puzzleInput(0), 100, puzzleInput.length)
  prettyPrint(finalMap, finalMap(1), "")
}

def solvePart2(puzzleInput: Array[Int]): Long = {
  val initialMap = initializeCupStats(puzzleInput ++ (10 to 1000000).toArray)
  val finalMap = simulateCupsEfficiently(initialMap, puzzleInput(0), 10000000, initialMap.size)
  val var1 = finalMap(1)
  val var2 = finalMap(var1)
  var1.toLong * var2.toLong
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_23.txt"

val puzzleInput = readInputFile(filepath)

solvePart1(puzzleInput)
solvePart2(puzzleInput)