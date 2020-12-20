import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

abstract class Rule
case class Singleton(ch: Char) extends Rule
case class RuleCombo(pats: List[Rule]) extends Rule
case class RuleOption(ops: (Rule, Rule)) extends Rule


def matchRule(rule: Rule, str: String, curMatch: String): (Boolean, String) = {
  @tailrec
  def matchList(rules: List[Rule], str: String, totalMatch: String): (Boolean, String) = {
    rules match {
      case hd::tail => {
        val (hasMatch, matched) = matchRule(hd, str, "")
        if (hasMatch) matchList(tail, str.stripPrefix(matched), totalMatch ++ matched)
        else (false, "")
      }
      case _ => (true, totalMatch)
    }
  }
  rule match {
    case Singleton(ch) => {
      val firstChar = str.headOption.getOrElse("!")
      if (firstChar == ch) (true, firstChar.toString) else (false, curMatch)
    }
    case RuleCombo(rls) => {
      matchList(rls, str, curMatch)
    }
    case RuleOption((l, r)) =>
      val (hasMatchL, matchedL) = matchRule(l, str, curMatch)
      if (hasMatchL) (hasMatchL, matchedL)
      else {
        matchRule(r, str, curMatch)
      }
  }
}

@tailrec
def extractRules(puzzleInput: List[String], rulesList: List[String]): (List[String], List[String]) = {
  if (puzzleInput.head == "") (rulesList, puzzleInput.tail)
  else
    extractRules(puzzleInput.tail, rulesList :+ puzzleInput.head)
}

def createRulesDict(puzzleRules: List[String]): Map[Int, Rule] = {
  @tailrec
  def createRulesDictLoop(puzzleRulesLeft: List[String], rulesDict: Map[Int, Rule],
                          toTreatAcc: List[String]): (Map[Int, Rule], List[String]) = {
    puzzleRulesLeft match {
      case hd::tl => {
        val sp = hd.split(": ")
        val (left, right) = (sp(0).toInt, sp(1))
        val check1 = new Regex ("[0-9]+").findAllMatchIn(right).toList.map(x => x.toString().toInt)
        val notReady = check1.exists(v => rulesDict.getOrElse(v, -1) == -1)
        if (notReady) createRulesDictLoop(tl, rulesDict, toTreatAcc :+ hd)
        else {
          if (right == "\"a\"") createRulesDictLoop(tl, rulesDict + (left -> Singleton('a')), toTreatAcc)
          else if (right == "\"b\"") createRulesDictLoop(tl, rulesDict + (left -> Singleton('b')), toTreatAcc)
          else {
            val opt = right.split(" \\| ")
            if (opt.length == 1) createRulesDictLoop(tl,
              rulesDict + (left -> RuleCombo(check1.map(rulesDict(_)))), toTreatAcc)
            else {
              val (ruleLeft, ruleRight) = (RuleCombo(opt(0).split(" ").map(x => rulesDict(x.toInt)).toList),
                RuleCombo(opt(1).split(" ").map(x => rulesDict(x.toInt)).toList))
              createRulesDictLoop(tl, rulesDict + (left -> RuleOption(ruleLeft, ruleRight)), toTreatAcc)
            }
          }
        }
      }
      case _ => (rulesDict, toTreatAcc)
    }
  }
  @tailrec
  def recursiveDictCreator(puzzleRules: List[String], rulesDict: Map[Int, Rule]): Map[Int, Rule] = {
    val (newDict, left) = createRulesDictLoop(puzzleRules, rulesDict, List())
    if (left.isEmpty) newDict
    else recursiveDictCreator(left, newDict)
  }
  recursiveDictCreator(puzzleRules, Map())
}

def solvePart1(strings: List[String], rule: Rule): Int = {
  def validateString(str: String): Boolean = {
    val (bool, acc) = matchRule(rule, str, "")
    bool & (acc == str)
  }
  strings.count(validateString)
}

def solvePart1Set(strings: List[String], rule: Rule): Set[String] = {
  def validateString(str: String): Boolean = {
    val (bool, acc) = matchRule(rule, str, "")
    bool & (acc == str)
  }
  strings.filter(validateString).toSet
}

def replaceLoopRules(rules: Map[Int, Rule]): (List[Rule], List[Rule]) = {
  def unfoldRuleLoop8(rulePossib: List[Rule], listLeft: List[Rule], accEl: List[Rule]): List[Rule] = {
    listLeft match {
      case hd::tl => {
        unfoldRuleLoop8(rulePossib :+ RuleCombo(accEl :+ hd), tl, accEl :+ hd)
      }
      case _ => rulePossib
    }
  }
  def unfoldRuleLoop11(rulePossib: List[Rule], ix: Int, accEl: List[Rule]): List[Rule] = {
    if (ix == 0) rulePossib
    else {
      val newEl = rules(42) +: accEl :+ rules(31)
      unfoldRuleLoop11(rulePossib :+ RuleCombo(newEl), ix-1, newEl)
    }
  }
  val rule8List = unfoldRuleLoop8(List(rules(42)), List.fill(5)(rules(42)), List(rules(42)))
  val rule11List = unfoldRuleLoop11(List(RuleCombo(List(rules(42), rules(31)))), 5, List(rules(42), rules(31)))
  (rule8List, rule11List)
}

def solvePart2(strings: List[String], rules: Map[Int, Rule]): Int = {
  val (rule8List, rule11List) = replaceLoopRules(rules)
  val sets = for (r8 <- rule8List; r11 <- rule11List) yield solvePart1Set(strings, RuleCombo(List(r8, r11)))
  sets.reduce(_++_).size
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_19.txt"

val (rulesRaw, strList) = extractRules(readInputFile(filepath), List.empty[String])
val rules = createRulesDict(rulesRaw)

solvePart1(strList, rules(0))
solvePart2(strList, rules)