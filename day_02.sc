import scala.io.Source

case class PuzzleInput(minLetters: Int, maxLetters: Int, letter: Char, password: String)

def parseInputLine(line: String): PuzzleInput = {
  val splittedLine = line.split(" ")
  val lims = splittedLine(0).split('-')
  val letter = splittedLine(1).head
  PuzzleInput(lims(0).toInt, lims(1).toInt, letter, splittedLine(2))
}

def readInputFile(filename: String): List[PuzzleInput] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.map(parseInputLine).toList
  testTxtSource.close()
  ret
}

def checkValidity1(inputLine: PuzzleInput): Boolean = {
  val count = inputLine.password.count(c => c == inputLine.letter)
  (inputLine.minLetters <= count) && (count <= inputLine.maxLetters)
}

def checkValidity2(inputLine: PuzzleInput): Boolean = {
  val maxIndex = inputLine.password.length
  val checkLeft =
    if (inputLine.minLetters <= maxIndex)
      inputLine.password(inputLine.minLetters - 1) == inputLine.letter
    else false
  val checkRight =
    if (inputLine.maxLetters <= maxIndex)
      inputLine.password(inputLine.maxLetters - 1) == inputLine.letter
    else false

  checkLeft ^ checkRight
}

def solvePart1(puzzle_input: List[PuzzleInput]): Int =
  puzzle_input.count(checkValidity1)

def solvePart2(puzzle_input: List[PuzzleInput]): Int =
  puzzle_input.count(checkValidity2)

val filepath = "your_path_here\\inputs\\input_2.txt"

val puzzleInput = readInputFile(filepath)

solvePart1(puzzleInput)
solvePart2(puzzleInput)