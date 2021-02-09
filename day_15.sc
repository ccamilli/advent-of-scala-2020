import scala.annotation.tailrec
import scala.io.Source

def readInputFile(filename: String): List[Int] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret.head.split(',').map(x => x.toInt).toList
}

def initDict(seq: List[Int]): Map[Int, (Int, Int)] = {
  @tailrec
  def initDictLoop(seqLeft: List[Int], map: Map[Int, (Int, Int)], num: Int): Map[Int, (Int, Int)] = {
    seqLeft match {
      case hd::tl => initDictLoop(tl, map + (hd -> (num, num)), num+1)
      case _ => map
    }
  }
  initDictLoop(seq, Map(), 1)
}

@tailrec
def solveProblem(map: Map[Int, (Int, Int)], lastSpoken: Int, roundNum: Int, nRounds: Int): Int = {
  if (roundNum == nRounds) lastSpoken
  else {
    val rnd = map(lastSpoken)
    val toSpeak = rnd._2 - rnd._1
    val mapUpdate = map.getOrElse(toSpeak, (roundNum, roundNum))
    solveProblem(map + (toSpeak -> (mapUpdate._2, roundNum)), toSpeak, roundNum + 1, nRounds)
  }
}

def solvePart1(initialSeq: List[Int]): Int = {
  solveProblem(initDict(initialSeq), initialSeq.takeRight(1).head, initialSeq.size+1, 2021)
}

def solvePart2(initialSeq: List[Int]): Int = {
  solveProblem(initDict(initialSeq), initialSeq.takeRight(1).head, initialSeq.size+1, 30000001)
}

val filepath = "your_path_here\\inputs\\input_15.txt"

solvePart1(readInputFile(filepath))
solvePart2(readInputFile(filepath))