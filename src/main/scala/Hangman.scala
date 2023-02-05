/**
 * Author: Richard B. Opsal, Ph.D.
 * Update: 2023/02/04
**/


///
// Helper functions for Hangman application.
///

object Helpers {

  // Generate list of possible words from passed file.
  // Empty list means nothing was loaded.
  def wordList(fname : String = "dictionaryWords.txt") : List[String] = {
    scala.util.Using(io.Source.fromFile( fname )){ source =>
      source.getLines.toList
    }.fold(_ => List.empty[String], identity)
  }

  // Return a random word from the passed list.
  extension (words : List[String]) def randomWord : String = {
    words( scala.util.Random.nextInt(words.length) )
  }

  // Split the word into individual letters plus upper case letters.
  extension (word : String) def splitWordToUpper : List[Char] = {
    word.toUpperCase.toList
  }

  // Join the list of characters together with a space in-between.
  extension (wordlist : List[Char]) def joinWord : String = {
    wordlist.mkString(" ")
  }

  // Set of upper case letters.
  val alphaSet : Set[Char] = ('A' to 'Z').toSet
}


///
// State case classes and associated routines.
///

object State {
  import Helpers.*

  case class Words(hangWord: String, words: List[String]) {
    def nextWord: Words = {
      val newHangWord = this.words.randomWord
      Words(newHangWord, this.words.filterNot(_ == newHangWord))
    }
  }

  case class WinsAndLosses(wins: Int = 0, losses: Int = 0) {
    def addLoss: WinsAndLosses = this.copy(losses = this.losses + 1)
    def  addWin: WinsAndLosses = this.copy(wins = this.wins + 1)
  }

  enum Status {
    case Win, Loss, Match, NoMatch, NoChange
  }

  private val maxGuesses = 6
  case class Guess(hangList: List[Char], guessList: List[Char], guessSet: Set[Char], guesses: Int) {
    // Input letter assumed to be uppercase.
    import Status.*, Guess.applyGuess
    def nextGuess: Char => (Status, Guess) = { letter =>
      if (guessSet.contains(letter)) {
        (NoChange, this)
      } else {
        val `match` = this.hangList.contains(letter)
        val newGuess = this.copy(
          guessList = if `match` then letter.applyGuess(this.guessList, this.hangList) else this.guessList,
          guessSet = this.guessSet + letter,
          guesses = if `match` then this.guesses else if 0 < this.guesses then this.guesses - 1 else 0
        )
        val status =
          if newGuess.hangList == newGuess.guessList then Win
          else if 0 == newGuess.guesses then Loss
          else if `match` then Match
          else NoMatch
        (status, newGuess)
      }
    }

    def finishGuess: WinsAndLosses => WinsAndLosses = { winsAndLosses =>
      if this.guesses == maxGuesses then winsAndLosses else winsAndLosses.addLoss
    }
  }

  object Guess {
    extension (letter: Char) def applyGuess(guessList: List[Char], hangList: List[Char]): List[Char] = {
      guessList.zip(hangList).map((g, h) => if (letter == h) h else g)
    }

    val nextRound: Words => Guess = words => {
      Guess(
        hangList = words.hangWord.splitWordToUpper,
        guessList = List.fill[Char](words.hangWord.length)('_'),
        guessSet = Set.empty[Char],
        guesses = maxGuesses
      )
    }
  }
}


///
// The Hangman application.
///

@main def Hangman (args: String*): Unit = {

  import Helpers.*
  import State.*, State.Status.*
  import scala.io.StdIn.readLine

  // Formatting strings for nice output.
  val fmtsummary = "%s  Wins : %2d  Losses : %2d"
  val fmtinput   = "\t%s  [Guesses left : %2d ] Letter : "

  def printSummary(message: String, hangList: List[Char], winsAndLosses: WinsAndLosses): Unit = {
    println("\t" + hangList.joinWord + "\n")
    println(fmtsummary.format(message, winsAndLosses.wins, winsAndLosses.losses))
    println()
  }

  def enterGameR(words: Words, winsAndLosses: WinsAndLosses): Unit = {
    def playGameR(guess: Guess): (Boolean, WinsAndLosses) = {
      readLine(fmtinput.format(guess.guessList.joinWord, guess.guesses)).toUpperCase match {
        case "NEW" => (false, guess.finishGuess(winsAndLosses))
        case "EXIT" => (true, guess.finishGuess(winsAndLosses))
        case str: String if str.isEmpty => playGameR(guess)
        case entry: String => {
          val letter = entry.head
          if ((1 < entry.length) || !alphaSet.contains(letter)) {
            println(s"Not a valid guess -> $entry")
            playGameR(guess)

          } else {
            val (status, newGuess) = guess.nextGuess(letter)
            status match {
              case Win  =>
                val newWinsAndLosses = winsAndLosses.addWin
                printSummary("Congratulations on your win!", guess.hangList, newWinsAndLosses) ;
                (false, newWinsAndLosses)

              case Loss =>
                val newWinsAndLosses = winsAndLosses.addLoss
                printSummary("Too Bad! Please try again.", guess.hangList, newWinsAndLosses) ;
                (false, newWinsAndLosses)

              case _ => playGameR(newGuess)
            }
          }
        }
      }
    }

    val (fDone, newWinsAndLosses) = playGameR(Guess.nextRound(words))
    println()
    if !fDone then enterGameR(words.nextWord, newWinsAndLosses)
  }

  // List of words to guess from.
  val fname = if args.isEmpty then "src/resources/dictionaryWords.txt" else args(0)
  val words = Words("HANGMAN", Helpers.wordList(fname)).nextWord

  println("Welcome to the Hangman word guessing game.")
  println("Type 'Exit' to leave the game, 'New' for a new game.")
  println("Good luck!\n")
  enterGameR(words, WinsAndLosses())
  println("\nThank you for playing Scala Hangman!")
}