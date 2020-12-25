import scala.io.Source
import scala.math.floorMod
import scala.annotation.tailrec

def readInputFile(filename: String): List[Long] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret.map(x => x.toLong)
}

def getLoopSize(ref: Long) : Int = {
  @tailrec
  def getLoopSizeAcc(ref: Long, num: Long, curSize: Int): Int = {
    if (num == ref) curSize
    else getLoopSizeAcc(ref, floorMod(num * 7, 20201227), curSize + 1)
  }

  getLoopSizeAcc(ref, 7, 1)
}

def getEncryptionKey(subjectNumber: Long, loopSize: Int): Long = {
  @tailrec
  def getEncryptionKeyAcc(value: Long, subjectNumber: Long, loopSize: Int): Long = {
    if (loopSize == 0) value
    else getEncryptionKeyAcc(floorMod(value * subjectNumber, 20201227), subjectNumber, loopSize - 1)
    }
  getEncryptionKeyAcc(1, subjectNumber, loopSize)
}

def solvePart1(filepath: String): Long = {
  val puzzleInput = readInputFile(filepath)
  val (cardPublic, doorPublic) = (puzzleInput.head, puzzleInput.tail.head)
  val (cardLS, doorLS) = (getLoopSize(cardPublic), getLoopSize(doorPublic))
  val (cardEK, doorEK) = (getEncryptionKey(doorPublic, cardLS), getEncryptionKey(cardPublic, doorLS))

  assert(cardEK == doorEK)

  cardEK
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_25.txt"

solvePart1(filepath)