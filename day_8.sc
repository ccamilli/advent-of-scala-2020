import scala.io.Source
import scala.annotation.tailrec

case class Instruction(text: String, num: Int) {
  def swap: Instruction = {
    if (this.text == "jmp") Instruction("nop", this.num)
    else
      if (this.text == "nop") Instruction("jmp", this.num)
      else this
  }
}

def readInputFile(filename: String): Array[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toArray
  testTxtSource.close()
  ret
}

def parseLine(line: String): Instruction = {
  val listLine = line.split(" ")
  Instruction(listLine(0), listLine(1).toInt)
}

def parseInputFile(filename: String): Array[Instruction] = {
  readInputFile(filename).map(parseLine)
}

def readInstruction(instructions: Array[Instruction], curPos: Int, curAcc: Int): (Int, Int) = {
  val instruction = instructions(curPos)
  if (instruction.text == "nop") (curPos + 1, curAcc)
  else if (instruction.text == "acc") (curPos + 1, curAcc + instruction.num)
  else (curPos + instruction.num, curAcc)
}

def solvePart1(instructions: Array[Instruction]): Either[Int, Int] = {
  def solvePart1Acc(instructions: Array[Instruction], curPos: Int,
                    curAcc: Int, seen: Set[(Instruction, Int)]): Either[Int, Int] = {
    if (curPos >= instructions.size) Left(curAcc)
    else {
      if (seen.contains(instructions(curPos), curPos)) Right(curAcc)
      else {
        val (newPos, newAcc) = readInstruction(instructions, curPos, curAcc)
        solvePart1Acc(instructions, newPos, newAcc, seen ++ Set((instructions(curPos), curPos)))
      }
    }
  }
  solvePart1Acc(instructions, 0, 0, Set())
}


def solvePart2(filename: String): Int = {
  @tailrec
  def solvePart2Acc(instructions: Array[Instruction], curPos: Int): Int = {
    solvePart1(instructions updated(curPos, instructions(curPos).swap)) match {
      case Right(_) => solvePart2Acc(instructions, curPos + 1)
      case Left(acc) => acc
    }
  }
  solvePart2Acc(parseInputFile(filename), 0)
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_8.txt"

solvePart1(parseInputFile(filepath)).getOrElse(0)
solvePart2(filepath)