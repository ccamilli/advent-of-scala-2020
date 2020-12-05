import scala.io.Source
import scala.annotation.tailrec

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

def parseInput(inputArray: List[String]): Map[Int, Map[String, String]] = {
  @tailrec
  def parseInputAcc(inputArray: List[String], idCount: Int, outputs: Map[Int, Map[String, String]]): Map[Int, Map[String, String]] = {
    inputArray match {
      case r::rs => {
        if (r == "") parseInputAcc(rs, idCount + 1, outputs)
        else {
          val currentInfo = outputs getOrElse (idCount, Map.empty[String, String])
          val updatedInfo = r.split(" ").foldLeft(currentInfo)((map, el) => map + (el.split(":")(0) -> el.split(":")(1)))
          parseInputAcc(rs, idCount, outputs + (idCount -> updatedInfo))
        }
      }
      case _ => outputs
    }
  }
  parseInputAcc(inputArray, 0, Map.empty[Int, Map[String, String]])
}

def validatePassport1(passportInfo: Map[String, String], requiredFields: Set[String]): Boolean = {
  passportInfo.view.filterKeys(k => requiredFields.contains(k)).size == requiredFields.size
}

def validatePassport2(passportInfo: Map[String, String]): Boolean = {
  val listInts = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  val listChars = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
  val checkByr = (passportInfo.getOrElse("byr", "1900").toInt >= 1920) && (passportInfo.getOrElse("byr", "1900").toInt <= 2002)
  val checkIyr = (passportInfo.getOrElse("iyr", "1900").toInt >= 2010) && (passportInfo.getOrElse("iyr", "1900").toInt <= 2020)
  val checkEyr = (passportInfo.getOrElse("eyr", "1900").toInt >= 2020) && (passportInfo.getOrElse("eyr", "1900").toInt <= 2030)
  val checkHgt = {
    val hgt = passportInfo.getOrElse("hgt", "")
    if (hgt.length < 3) false else {
      val unit = hgt.takeRight(2)
      val scale = hgt.split(unit)(0).toInt
      if (unit == "in") (scale <= 76 && scale >= 59) else {
        if (unit == "cm") (scale <= 193 && scale >= 150) else
        false
      }
    }
  }
  val checkHcl = {
    val hcl = passportInfo.getOrElse("hcl", "")
    if (hcl.length != 7) false else {
      hcl(0) == '#' && (hcl.drop(1).count(x => listChars.contains(x)) == 6)
    }
  }
  val checkEcl = {
    val validEcls = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    val ecl = passportInfo.getOrElse("ecl", "asdasdas")
    validEcls.contains(ecl)
  }
  val checkPid = {
    val pid = passportInfo.getOrElse("pid", "asdasda")
    if (pid.length != 9) false else {
      pid.count(x => listInts.contains(x)) == 9
    }
  }
  checkByr && checkIyr && checkEyr && checkHgt && checkHcl && checkEcl && checkPid
}

def solvePart1(filename: String): Int = {
  val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val passportInfo = parseInput(readInputFile(filename))
  passportInfo.count{case (_, m) => validatePassport1(m, requiredFields)}
}

def solvePart2(filename: String): Int = {
  val passportInfo = parseInput(readInputFile(filename))
  passportInfo.count{case (_, m) => validatePassport2(m)}
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_4.txt"

solvePart1(filepath)
solvePart2(filepath)