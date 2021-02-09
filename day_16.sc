import scala.annotation.tailrec
import scala.io.Source

type ParsedInfo = Map[String, ((Int, Int), (Int, Int))]

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

def parseInfo(puzzleInputLeft: List[String],
              info: ParsedInfo): (List[String], ParsedInfo) = {
  val lineInfo = puzzleInputLeft.head
  if (lineInfo == "") (puzzleInputLeft.tail.tail, info)
  else {
    val lineInfoSplit = lineInfo.split(": ")
    val (str, ranges) = (lineInfoSplit.head, lineInfoSplit(1))
    val parsedRanges = ranges.split(" or ")
    val (rangeLeft, rangeRight) = (parsedRanges.head.split('-').map(_.toInt),
      parsedRanges(1).split('-').map(_.toInt))
    parseInfo(puzzleInputLeft.tail, info + (str -> ((rangeLeft(0), rangeLeft(1)), (rangeRight(0), rangeRight(1)))))
  }
}

def parseMyTicket(puzzleInputLeft: List[String]): (List[String], Array[Int]) = {
  (puzzleInputLeft.tail.tail.tail, puzzleInputLeft.head.split(',').map(_.toInt))
}

@tailrec
def parseNearbyTickets(puzzleInputLeft: List[String], nearbyTicketsInfo: List[Array[Int]]): List[Array[Int]] = {
  puzzleInputLeft match {
    case hd::tl => parseNearbyTickets(tl, nearbyTicketsInfo:::List(hd.split(',').map(_.toInt)))
    case _ => nearbyTicketsInfo
  }
}

def getPossibilities(overallInfo: ParsedInfo, transposed: List[List[Int]]): Map[Int, Set[String]] = {
  @tailrec
  def getPossibilitiesLoop(overallInfo: ParsedInfo, transposed: List[List[Int]],
                           mapCumul: Map[Int, Set[String]], i: Int): Map[Int, Set[String]] = {
    if (i == transposed.size) mapCumul
    else {
      val possibilities = overallInfo.filter{
        case (_, ((ll, lu), (rl, ru))) => transposed(i).forall(
          x => (ll <= x && lu >= x) || (rl <= x && ru >= x)
        )
      }.keys.toSet
      getPossibilitiesLoop(overallInfo, transposed, mapCumul + (i -> possibilities), i+1)
    }
  }
  getPossibilitiesLoop(overallInfo, transposed, Map(), 0)
}

def solvePart1(overallInfo: ParsedInfo, nearbyTicketsInfo: List[Array[Int]]): Int = {
  val overallValues = overallInfo.values
  nearbyTicketsInfo.flatten.filter(x => !overallValues.exists{
    case ((ll, lu), (rl, ru)) => (ll <= x && lu >= x) || (rl <= x && ru >= x)}).sum
}

def solvePart2(overallInfo: ParsedInfo, transposed: List[List[Int]], myTicketInfo: Array[Int]): Long = {
  @tailrec
  def recursiveSolver(possibilities: Map[Int, Set[String]], solved: Map[Int, String]): Map[Int, String] = {
    if (possibilities.size == solved.size) solved
    else {
      val onePossib = possibilities.filter{case (_, possib) => possib.size == 1}.map{
        case (k, v) => (k, v.head)
      }
      recursiveSolver(possibilities.map{
        case (k, v) => (k, v diff onePossib.values.toSet)
      }, solved ++ onePossib)
    }
  }

  val possibilities = getPossibilities(overallInfo, transposed)
  val ixs = recursiveSolver(possibilities, Map()).filter{
    case (_, v) => v.startsWith("departure")
  }.keys.toSet

  myTicketInfo.zipWithIndex.collect{case (x, i) if ixs.contains(i) => x.toLong}.product
}

val filepath = "your_path_here\\inputs\\input_16.txt"
val puzzleInput = readInputFile(filepath)

val (inputLeft, overallInfo) = parseInfo(puzzleInput, Map())
val (inputLeft2, myTicketInfo) = parseMyTicket(inputLeft)
val nearbyTicketsInfo = parseNearbyTickets(inputLeft2, List())

val validTickets = nearbyTicketsInfo.filter(arr => arr.forall(
  x => overallInfo.values.exists{
    case ((ll, lu), (rl, ru)) => (ll <= x && lu >= x) || (rl <= x && ru >= x)
  }))

solvePart1(overallInfo, nearbyTicketsInfo)
solvePart2(overallInfo, validTickets.transpose, myTicketInfo)