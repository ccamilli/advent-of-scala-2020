import scala.annotation.tailrec
import scala.io.Source

type Deck = List[Int]

def readInputFile(filename: String): List[String] = {
  val testTxtSource = Source.fromFile(filename)
  val ret = testTxtSource.getLines.toList
  testTxtSource.close()
  ret
}

@tailrec
def calculateScoreLoop(deckLeft: Deck, acc: Int, i: Int): Int = {
  deckLeft match {
    case hd::tl => calculateScoreLoop(tl, acc + hd*i, i+1)
    case _ => acc
  }
}

case class Game(player1: Deck, player2: Deck) {
  def play(): Deck = {
    if (player1.isEmpty) player2
    else if (player2.isEmpty) player1
    else {
      val (h1, h2) = (player1.head, player2.head)
      if (h1 > h2)
        Game(player1.tail :+ h1 :+h2, player2.tail).play()
      else
        Game(player1.tail, player2.tail :+ h2 :+h1).play()
    }
  }
}

case class RecursiveCombat(player1: Deck, player2: Deck, seenConfigurations: Set[(Int, Int)]) {
  def play(): (Int, Deck) = {
    val toAdd = (calculateScoreLoop(player1.reverse, 0, 1),
      calculateScoreLoop(player2.reverse, 0, 1))
    if (seenConfigurations.contains(toAdd)) (1, player1)
    else
      if (player1.isEmpty) (2, player2)
      else
        if (player2.isEmpty) (1, player1)
        else {
          val newConfigurations = seenConfigurations + toAdd
          val (h1, h2) = (player1.head, player2.head)
          val (winner, _) = {
            if (h1 <= player1.tail.size && h2 <= player2.tail.size) {
            RecursiveCombat(player1.tail.take(h1), player2.tail.take(h2), Set()).play()
          }
            else if (h1 > h2) (1, player1) else (2, player2)
          }
          if (winner == 1) RecursiveCombat(player1.tail :+ h1 :+h2, player2.tail, newConfigurations).play()
          else RecursiveCombat(player1.tail, player2.tail :+ h2 :+h1, newConfigurations).play()
        }
  }
}



def isolateDecks(puzzleInput: List[String]): (Deck, Deck) = {
  @tailrec
  def isolateDecksLoop(inputLeft: List[String], curDeck: Deck, allDecks: List[Deck]): List[Deck] = {
    inputLeft match {
      case hd::tl => {
        if (hd == "") isolateDecksLoop(tl, List(), allDecks :+ curDeck)
        else
          if (hd.startsWith("Player")) isolateDecksLoop(tl, curDeck, allDecks)
          else isolateDecksLoop(tl, curDeck :+ hd.toInt, allDecks)
      }
      case _ => allDecks :+ curDeck
    }
  }
  val decks = isolateDecksLoop(puzzleInput, List(), List())
  (decks.head, decks.tail.head)
}

def solvePart1(filepath: String): Int = {
  val (p1, p2) = isolateDecks(readInputFile(filepath))
  val finalDeck = Game(p1, p2).play()
  calculateScoreLoop(finalDeck.reverse, 0, 1)
}

def solvePart2(filepath: String): Int = {
  val (p1, p2) = isolateDecks(readInputFile(filepath))
  val (_, deck) = RecursiveCombat(p1, p2, Set()).play()
  calculateScoreLoop(deck.reverse, 0, 1)
}

val filepath = "C:\\Users\\c.camilli\\OneDrive - CRITEO\\PERSONNEL\\Advent of code 2020\\inputs\\input_22.txt"

solvePart1(filepath)
solvePart2(filepath)