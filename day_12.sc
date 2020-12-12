import scala.annotation.tailrec
import scala.io.Source
import scala.math.floorMod

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

case class ShipPosition(pos: (Int, Int), orientation: Int) {
  def readInstruction(instr: String): ShipPosition = {
    val action = instr(0)
    val num = instr.drop(1).toInt
    val (x, y) = pos
    if (action == 'N') ShipPosition((x, y+num), orientation)
    else if (action == 'S') ShipPosition((x, y-num), orientation)
    else if (action == 'W') ShipPosition((x-num, y), orientation)
    else if (action == 'E') ShipPosition((x+num, y), orientation)
    else if (action == 'L') ShipPosition((x, y), floorMod(orientation + num, 360))
    else if (action == 'R') ShipPosition((x, y), floorMod(orientation - num, 360))
    else {
      if (orientation == 0) ShipPosition((x+num, y), orientation)
      else if (orientation == 90) ShipPosition((x, y+num), orientation)
      else if (orientation == 180) ShipPosition((x-num, y), orientation)
      else ShipPosition((x, y-num), orientation)
    }
  }
}

case class ShipPosition2(pos: (Int, Int), waypoint: (Int, Int)) {
  def readInstruction(instr: String): ShipPosition2 = {
    val action = instr(0)
    val num = instr.drop(1).toInt
    val (x, y) = pos
    val (wayX, wayY) = waypoint
    if (action == 'N') ShipPosition2((x, y), (wayX, wayY + num))
    else if (action == 'S') ShipPosition2((x, y), (wayX, wayY - num))
    else if (action == 'W') ShipPosition2((x, y), (wayX - num, wayY))
    else if (action == 'E') ShipPosition2((x, y), (wayX + num, wayY))
    else if (action == 'L' || action == 'R') ShipPosition2((x, y),
      crazyRotation(waypoint, action, num))
    else {
      ShipPosition2((x + num*wayX, y + num*wayY), (wayX, wayY))
    }
  }

  def crazyRotation(waypoint: (Int, Int), direction: Char, angle: Int): (Int, Int) = {
    if (angle == 0) waypoint
    else {
      val (wayX, wayY) = waypoint
      val newPos = if (direction == 'L') (-wayY, wayX) else (wayY, -wayX)
      crazyRotation(newPos, direction, angle-90)
    }
  }
}

def solvePart1(instrList: List[String]): Int = {
  @tailrec
  def solvePart1Loop(instrList: List[String], currentPos: ShipPosition): Int = {
    instrList match {
      case hd::tl => solvePart1Loop(tl, currentPos.readInstruction(hd))
      case _ => currentPos.pos._1.abs + currentPos.pos._2.abs
    }
  }
  solvePart1Loop(instrList, ShipPosition((0, 0), 0))
}

def solvePart2(instrList: List[String]): Int = {
  @tailrec
  def solvePart2Loop(instrList: List[String], currentPos: ShipPosition2): Int = {
    instrList match {
      case hd::tl => solvePart2Loop(tl, currentPos.readInstruction(hd))
      case _ => currentPos.pos._1.abs + currentPos.pos._2.abs
    }
  }
  solvePart2Loop(instrList, ShipPosition2((0, 0), (10, 1)))
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_12.txt"
val inputList = readInputFile(filepath)
solvePart1(inputList)
solvePart2(inputList)

