import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

def convertToBitseq(num: BigInt): String = {
  num.toString(2).reverse.padTo(36, '0').reverse
}

def parseBigInt(str: String): BigInt = {
  def parseBigIntLoop(strLeft: String, acc: BigInt, power: BigInt): BigInt = {
    if (strLeft == "") acc
    else parseBigIntLoop(strLeft.dropRight(1), acc + power*strLeft.takeRight(1).toInt, power*2)
  }
  parseBigIntLoop(str, 0, 1)
}

def mergeMask(num: BigInt, mask: String): BigInt = {
  @tailrec
  def mergeMaskLoop(bitseq: String, mask: String, result: String, i: Int): String = {
    if (i == 36) result
    else {
      val ch = if (mask(i) == 'X') bitseq(i) else mask(i)
      mergeMaskLoop(bitseq, mask, result + ch, i+1)
    }
  }
  parseBigInt(mergeMaskLoop(convertToBitseq(num), mask, "", 0))
}

def materializeXBits(mergedAddress: String): List[BigInt] = {
  def getAllStrings(mergedAddresses: String, result: List[String], pos: Int): List[String] = {
    if (pos == 36) result
    else {
      val curChar = mergedAddresses(pos)
      if (curChar == 'X')
        getAllStrings(mergedAddresses, result.map(x => x + "0"), pos + 1) ++
          getAllStrings(mergedAddresses, result.map(x => x + "1"), pos + 1)
      else
        getAllStrings(mergedAddresses, result.map(x => x + curChar), pos + 1)
    }
  }
  getAllStrings(mergedAddress, List(""), 0).map(x => parseBigInt(x))
}

def mergeMaskAddress(addressNum: BigInt, mask: String): String = {
  @tailrec
  def mergeMaskAddLoop(bitseq: String, mask: String, result: String, i: Int): String = {
    if (i == 36) result
    else {
      val ch = if (mask(i) == '0') bitseq(i) else mask(i)
      mergeMaskAddLoop(bitseq, mask, result + ch, i+1)
    }
  }
  mergeMaskAddLoop(convertToBitseq(addressNum), mask, "", 0)
}

def solvePart1(puzzleInput: List[String]): BigInt = {
  def solvePart1Loop(inputLeft: List[String], curMask: String, out: Map[BigInt, BigInt]): Map[BigInt, BigInt] = {
    inputLeft match {
      case hd::tl => {
        if (hd.startsWith("mask")) solvePart1Loop(tl, hd.split(" = ")(1), out)
        else {
          val matches = new Regex ("[0-9]+").findAllMatchIn(hd).toList.map(x => BigInt(x.toString()))
          solvePart1Loop(tl, curMask, out + (matches.head -> mergeMask(matches(1), curMask)))
        }
      }
      case _ => out
    }
  }
  solvePart1Loop(puzzleInput, "X"*36, Map.empty[BigInt, BigInt]).values.sum
}

def solvePart2(puzzleInput: List[String]): BigInt = {
  def solvePart2Loop(inputLeft: List[String], curMask: String, out: Map[BigInt, BigInt]): Map[BigInt, BigInt] = {
    inputLeft match {
      case hd::tl => {
        if (hd.startsWith("mask")) solvePart2Loop(tl, hd.split(" = ")(1), out)
        else {
          val matches = new Regex ("[0-9]+").findAllMatchIn(hd).toList.map(x => BigInt(x.toString()))
          val addresses = materializeXBits(mergeMaskAddress(matches.head, curMask))
          solvePart2Loop(tl, curMask,
            addresses.foldLeft(out)((dic, addr) => dic + (addr -> matches(1))))
        }
      }
      case _ => out
    }
  }
  solvePart2Loop(puzzleInput, "X"*36, Map.empty[BigInt, BigInt]).values.sum
}

val filepath = "your_path_here\\inputs\\input_14.txt"

solvePart1(readInputFile(filepath))
solvePart2(readInputFile(filepath))