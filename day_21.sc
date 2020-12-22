import scala.annotation.tailrec
import scala.io.Source

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

type Ingredient = String
type Allergen = String

def parseRow(str: String): (Set[Ingredient], Set[Allergen]) = {
  val splitted = str.split(" \\(")
  val (ingr, all) = (splitted.head, splitted(1))
  val ingrList = ingr.split(" ").toSet
  val allList = all.dropRight(1).split("contains ")(1).split(", ").toSet
  (ingrList, allList)
}

def excludePossibilitiesRow(row: (Set[Ingredient], Set[Allergen]),
                         allIngr: Set[Ingredient]): List[(Ingredient, Allergen)] = {
  val (ingrs, alrgs) = row
  val notPresent = allIngr diff ingrs
  (for (ingr <- notPresent; alrg <- alrgs) yield (ingr, alrg)).toList
}

def excludePossibilities(parsedInput: List[(Set[Ingredient], Set[Allergen])]): Map[Ingredient, Set[Allergen]] = {
  val allIngredients = parsedInput.map(_._1).reduce(_ ++ _)
  val exclRaw = parsedInput.flatMap(row => excludePossibilitiesRow(row, allIngredients))

  exclRaw.groupBy(x => x._1).map {
    case (k, v) => (k, v.map(_._2).toSet)}
  }

def solvePart1(parsedInput: List[(Set[Ingredient], Set[Allergen])]): Int = {
  val allAllergens = parsedInput.map(_._2).reduce(_ ++ _)
  val noAllergies = excludePossibilities(parsedInput).filter(x => x._2.size == allAllergens.size).keys.toSet

  parsedInput.map {
    case (ingrs, _) => (noAllergies intersect ingrs).size
  }.sum
}

def solvePart2(parsedInput: List[(Set[Ingredient], Set[Allergen])]): String = {
  @tailrec
  def solvePart2Loop(currInput: List[(Set[Ingredient], Set[Allergen])],
                     answer: Map[Ingredient, Allergen],
                     fullAlergens: Set[Allergen]): Map[Ingredient, Allergen] = {
  if (answer.size == fullAlergens.size) answer
  else {
    val allAllergens = currInput.map(_._2).reduce(_ ++ _)

    val mapSolvedRightAway = currInput.filter {case (ingrs, alrgs) => ingrs.size == 1 && alrgs.size == 1}
      .map {case (ingrs, alrgs) => (ingrs.head, alrgs.head)}.toMap

    val stillPossible = excludePossibilities(currInput).map {
      case (ingr, allerg) => (ingr, allAllergens diff allerg)
    }

    val impossible = stillPossible.filter {case (_, v) => v.isEmpty}

    val solved = stillPossible.filter {case (_, v) => v.size == 1}
      .map {case (k, v) => (k, v.head)} ++ mapSolvedRightAway

    val ingrToRemove = impossible.keys.toSet ++ solved.keys.toSet
    val alrgsToRemove = solved.values.toSet

    val nextInput = currInput.map {
      case (ingrs, alrgs) => (ingrs diff ingrToRemove, alrgs diff alrgsToRemove)
    }
    solvePart2Loop(nextInput, answer ++ solved, fullAlergens)
    }
  }

  solvePart2Loop(parsedInput, Map(),
    parsedInput.map(_._2).reduce(_ ++ _)).toList.sortBy(_._2).map(_._1).reduce(_ ++ "," ++ _)
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_21.txt"

solvePart1(readInputFile(filepath).map(parseRow))
solvePart2(readInputFile(filepath).map(parseRow))