import scala.io.Source
import scala.annotation.tailrec

def readInputFile(filename: String): List[Int] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.map(x => x.toInt).toList
  testTxtSource.close()
  ret
}

def twoSum(vals: List[Int], target: Int): Option[(Int, Int)] = {
  @tailrec
  def twoSumAcc(vals_left: List[Int], target: Int, seen: Set[Int]): Option[(Int, Int)] = {
    vals_left match {
      case v::vs => if (seen.contains(target - v)) Some((v, target - v)) else twoSumAcc(vs, target, seen + v)
      case _ => None
    }
  }
  twoSumAcc(vals, target, Set.empty)
}

def threeSum(vals: List[Int], target: Int): Option[(Int, Int, Int)] = {
  @tailrec
  def threeSumAcc(valsLeft: List[Int], target: Int, seen: Set[Int]): Option[(Int, Int, Int)] = {
    valsLeft match {
      case v::vs => {
        twoSum(vs, target-v) match {
          case Some((a, b)) => Some((v, a, b))
          case None => threeSumAcc(vs, target, seen + v)
        }
      }
      case _ => None
    }
  }
  threeSumAcc(vals, target, Set.empty)
}

def solvePart1(input: List[Int]): Int = {
  val a = twoSum(input, 2020).get
  a._1 * a._2
}

def solvePart2(input: List[Int]): Int = {
  val a = threeSum(input, 2020).get
  a._1 * a._2 * a._3
}

val filepath = "your_path_here\\inputs\\input_1.txt"

val puzzleInput = readInputFile(filepath)

solvePart1(puzzleInput)
solvePart2(puzzleInput)

