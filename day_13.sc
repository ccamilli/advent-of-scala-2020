import scala.annotation.tailrec
import scala.io.Source
import scala.math.floorMod

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

def solvePart1(puzzleInput: List[String]): Int = {
  val ts = puzzleInput.head.toInt
  val buses = puzzleInput(1).split(',').filter(x => x != "x").map(x => (x.toInt, x.toInt - ts % x.toInt))
  val min = buses.map(x => x._2).min
  buses.filter(x => x._2 == min).head._1 * min
}

def computeBezoutCoeffs(a: BigInt, b: BigInt): (BigInt, BigInt) = {
  @tailrec
  def computeBezoutCoeffsLoop(oldR: BigInt, r: BigInt, oldS: BigInt,
                              s: BigInt, oldT: BigInt, t: BigInt): (BigInt, BigInt) = {
    if (r == 0) (oldS, oldT)
    else {
      val qt = oldR/r
      computeBezoutCoeffsLoop(r, oldR - qt * r, s, oldS - qt * s, t, oldT - qt * t)
    }
  }
  computeBezoutCoeffsLoop(a, b, 1, 0, 0, 1)
}

def reduceModularSystem(left: (BigInt, BigInt), right: (BigInt, BigInt)): (BigInt, BigInt) = {
  val (n1, a1) = left
  val (n2, a2) = right
  val (m1, m2) = computeBezoutCoeffs(n1, n2)
  (n1*n2, (a1*m2*n2 + a2*m1*n1).mod(n1*n2))
}

def solvePart2(puzzleInput: List[String]): BigInt = {
  @tailrec
  def getModularSystem(buses: List[String], acc: List[(BigInt, BigInt)], ix: Int): List[(BigInt, BigInt)] = {
    buses match {
      case hd::tail =>
        if (hd != "x") getModularSystem(tail, acc ++ List((hd.toInt, floorMod(-ix, hd.toInt))), ix + 1)
        else getModularSystem(tail, acc, ix + 1)
      case _ => acc
    }
  }
  getModularSystem(puzzleInput(1).split(',').toList, List(), 0).reduce(reduceModularSystem)._2
}

val filepath = "your_path_here\\inputs\\input_13.txt"

val puzzleInput = readInputFile(filepath)

solvePart1(puzzleInput)
solvePart2(puzzleInput)

