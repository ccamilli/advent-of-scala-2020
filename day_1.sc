import scala.io.Source
import scala.collection.Set
import scala.annotation.tailrec

def read_input_file(filename: String): List[Int] = {
  Source.fromFile(filename).getLines.map(x => x.toInt).toList
}

def two_sum(vals: List[Int], target: Int): Option[(Int, Int)] = {
  @tailrec
  def two_sum_acc(vals_left: List[Int], target: Int, seen: Set[Int]): Option[(Int, Int)] = {
    vals_left match {
      case v::vs => if (seen.contains(target - v)) Some((v, target - v)) else two_sum_acc(vs, target, seen ++ Set(v))
      case _ => None
    }
  }
  two_sum_acc(vals, target, Set.empty)
}

def three_sum(vals: List[Int], target: Int): Option[(Int, Int, Int)] = {
  @tailrec
  def three_sum_acc(vals_left: List[Int], target: Int, seen: Set[Int]): Option[(Int, Int, Int)] = {
    vals_left match {
      case v::vs => {
        two_sum(vs, target-v) match {
          case Some((a, b)) => Some((v, a, b))
          case None => three_sum_acc(vs, target, seen ++ Set(v))
        }
      }
      case _ => None
    }
  }
  three_sum_acc(vals, target, Set.empty)
}

def solve_part_1(input: List[Int]): Int = {
  val a = two_sum(input, 2020).get
  a._1 * a._2
}

def solve_part_2(input: List[Int]): Int = {
  val a = three_sum(input, 2020).get
  a._1 * a._2 * a._3
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_1.txt"

val puzzle_input = read_input_file(filepath)

solve_part_1(puzzle_input)
solve_part_2(puzzle_input)

