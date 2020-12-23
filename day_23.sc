import scala.io.Source
import scala.math.floorMod
import scala.collection.mutable

def readInputFile(filename: String): Array[Int] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret.head.map(x => x.toString.toInt).toArray
}

def simulateCupsEfficiently(cupStats: mutable.Map[Int, (Int, Int)], curEl: Int,
                            movesLeft: Int, modulo: Int): mutable.Map[Int, (Int, Int)] = {
  if (movesLeft == 0) cupStats
  else {
    val r1 = cupStats(curEl)._2
    val r2 = cupStats(r1)._2
    val r3 = cupStats(r2)._2
    val newRight = cupStats(r3)._2

    cupStats(newRight) = (curEl, cupStats(newRight)._2)
    cupStats(curEl) = (cupStats(curEl)._1, newRight)

    val elems = Set(r1, r2, r3)

    val targetLabel = {
      if (!elems.contains(floorMod(curEl - 2, modulo) + 1)) floorMod(curEl - 2, modulo) + 1
      else
        if (!elems.contains(floorMod(curEl - 3, modulo) + 1)) floorMod(curEl - 3, modulo) + 1
        else
          if (!elems.contains(floorMod(curEl - 4, modulo) + 1)) floorMod(curEl - 4, modulo) + 1
          else floorMod(curEl - 5, modulo) + 1
    }
    val (lbLeft, lbRight) = cupStats(targetLabel).copy()

    cupStats(targetLabel) = (lbLeft, r1)
    cupStats(r1) = (targetLabel, r2)

    cupStats(r3) = (r2, lbRight)
    cupStats(lbRight) = (r3, cupStats(lbRight)._2)

    simulateCupsEfficiently(cupStats, cupStats(curEl)._2, movesLeft - 1, modulo)

  }
}

def initializeCupStats(puzzleInput: Array[Int]): Map[Int, (Int, Int)] = {
  val mod = puzzleInput.length
  puzzleInput.zipWithIndex.map{
    case (el, ix) => (el, (puzzleInput(floorMod(ix - 1, mod)), puzzleInput(floorMod(ix + 1, mod))))
  }
}.toMap

def prettyPrint(map: mutable.Map[Int, (Int, Int)], curVal: Int, cumul: String): String = {
  if (curVal == 1) cumul
  else prettyPrint(map, map(curVal)._2, cumul + curVal)
}

def solvePart1(puzzleInput: Array[Int]): String = {
  val initialMutableMap = mutable.Map(initializeCupStats(puzzleInput).toSeq: _*)
  val finalMap = simulateCupsEfficiently(initialMutableMap, puzzleInput(0), 100, puzzleInput.length)
  prettyPrint(finalMap, finalMap(1)._2, "")
}

def solvePart2(puzzleInput: Array[Int]): Long = {
  val initialMutableMap = mutable.Map(initializeCupStats(puzzleInput ++ (10 to 1000000).toArray).toSeq: _*)
  val finalMap = simulateCupsEfficiently(initialMutableMap, puzzleInput(0), 10000000, initialMutableMap.size)
  val var1 = finalMap(1)._2
  val var2 = finalMap(var1)._2
  var1.toLong * var2.toLong
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_23.txt"

val puzzleInput = readInputFile(filepath)

solvePart1(puzzleInput)
solvePart2(puzzleInput)


