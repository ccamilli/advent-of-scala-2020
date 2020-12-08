import scala.io.Source

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

def treatContainedList(contained: String): Option[(Int, String)] = {
  val stripped = contained.split(" ").take(3)
  if (stripped(0) == "no") None else Some(stripped(0).toInt, stripped(1) + " " + stripped(2))
}

def parseLine(line: String): (String, List[(Int, String)]) = {
  val lineWords = line.split(" contain ")
  val container = lineWords(0).split(" bags")(0)
  val containedList = lineWords(1).split(", ").flatMap(treatContainedList).toList
  (container, containedList)
}

def buildGraph(filename: String): Map[String, List[(Int, String)]] = {
  readInputFile(filename).foldLeft(Map.empty[String, List[(Int, String)]])((m, el) => m ++ List(parseLine(el)))
}

def graphSearch(graph: Map[String, List[(Int, String)]], item: String): Int = {
  def graphSearchAcc(graph: Map[String, List[(Int, String)]], item: String): Set[String] = {
    val firstLevelConnections = graph.filter { case (_, listConnection) => listConnection.map {
      case (_, contained) => contained}.toSet.contains(item)
    }.keys
    if (firstLevelConnections.isEmpty) Set.empty[String] else
      firstLevelConnections.map(x => graphSearchAcc(graph, x) ++ Set(x)).reduce(_ ++ _)
  }
  graphSearchAcc(graph, item).size
}

def graphSearch2(graph: Map[String, List[(Int, String)]], item: String): Int = {
  val directConnections = graph.getOrElse(item, List.empty)
  directConnections.map{case (qt, bag) => qt * (1 + graphSearch2(graph, bag))}.sum
}

def solvePart1(filename: String) = {
  graphSearch(buildGraph(filename), "shiny gold")
}

def solvePart2(filename: String) = {
  graphSearch2(buildGraph(filename), "shiny gold")
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_7.txt"

solvePart1(filepath)
solvePart2(filepath)