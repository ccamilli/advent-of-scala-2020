import scala.annotation.tailrec
import scala.io.Source

def readInputFile(filename: String): List[Int] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret.head.split(',').map(x => x.toInt).toList
}

def initDict(seq: List[Int]): Map[Int, (Int, Int)] = {
  def initDictLoop(seqLeft: List[Int], map: Map[Int, (Int, Int)], num: Int): Map[Int, (Int, Int)] = {
    seqLeft match {
      case hd::tl => initDictLoop(tl, map + (hd -> (num, num)), num+1)
      case _ => map
    }
  }
  initDictLoop(seq, Map(), 1)
}

def solvePart1(initialSeq: List[Int]): Int = {
  @tailrec
  def solvePart1Loop(map: Map[Int, (Int, Int)], lastSpoken: Int, roundNum: Int): Int = {
    if (roundNum == 2021) {
      lastSpoken
    }
    else {
      val rnd = map(lastSpoken)
      val toSpeak = rnd._2 - rnd._1
      val mapUpdate = map.getOrElse(toSpeak, (roundNum, roundNum))
      solvePart1Loop(map + (toSpeak -> (mapUpdate._2, roundNum)), toSpeak, roundNum + 1)
    }
  }
  solvePart1Loop(initDict(initialSeq), initialSeq.takeRight(1).head, initialSeq.size+1)
}

def solvePart2(initialSeq: List[Int]): Int = {
  @tailrec
  def solvePart2Loop(map: Map[Int, (Int, Int)], lastSpoken: Int, roundNum: Int): Int = {
    if (roundNum == 30000001) {
      lastSpoken
    }
    else {
      val rnd = map(lastSpoken)
      val toSpeak = rnd._2 - rnd._1
      val mapUpdate = map.getOrElse(toSpeak, (roundNum, roundNum))
      solvePart2Loop(map + (toSpeak -> (mapUpdate._2, roundNum)), toSpeak, roundNum + 1)
    }
  }
  solvePart2Loop(initDict(initialSeq), initialSeq.takeRight(1).head, initialSeq.size+1)
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_15.txt"

solvePart1(readInputFile(filepath))
solvePart2(readInputFile(filepath))