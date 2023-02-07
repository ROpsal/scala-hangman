/**
 * Author: Richard B. Opsal, Ph.D.
 * Update: 2023/02/07
**/


import scala.annotation.tailrec
import java.text.Collator
import java.util.Locale


///
// Helper functions for Hangman application.
///

object Helpers {

  case class WordEntry(word: String, definition: Option[String])
  extension (line: String) def toWordEntry: Option[WordEntry] = {
    line.split('|') map (_.trim) match {
      case Array(word) if word.isEmpty => None
      case Array(word, _*) if word.startsWith("#") => None
      case Array(word, value) => Some(WordEntry(word, Some(value)))
      case Array(word) => Some(WordEntry(word, None))
    }
  }

  // Generate list of possible words from passed file.
  // Empty list means nothing was loaded.
  def wordList(fname : String = "dictionaryWords.txt") : List[WordEntry] = {
    scala.util.Using(io.Source.fromFile( fname )){ source =>
      source.getLines.toList
    }.fold(_ => List.empty[Option[WordEntry]], lines => lines.map(_.toWordEntry)).flatten
  }

  // Return a random word from the passed list.
  extension (words: List[WordEntry]) def randomWord : WordEntry = {
    words( scala.util.Random.nextInt(words.length) )
  }

  // Split the word into individual letters.
  extension (word: String) def splitWord : List[Char] = word.toList

  // Join the list of characters together with a space in-between.
  extension (wordlist: List[Char]) def joinWord : String = wordlist.mkString(" ")

  // French letters supported by the Hangman application.
  private val accentSet  : Set[Char] = Set('ç', 'é', 'â', 'ê', 'î', 'ô', 'û', 'à', 'è', 'ù', 'ë', 'ï', 'ü')
  private val ligatureSet: Set[Char] = Set('œ', 'æ')
  private val alphaSet   : Set[Char] = ('a' to 'z').toSet
  val frenchSet : Set[Char] = accentSet ++ alphaSet ++ ligatureSet

  // Compares characters without regard to case or French accent marks.
  val frCollator = Collator.getInstance(Locale.FRENCH)
  frCollator.setStrength(Collator.PRIMARY)
  extension (l: Char) infix def =:= (r: Char): Boolean = 0 == frCollator.compare(l.toString, r.toString)
  extension (l: String) infix def =:= (r: String): Boolean = 0 == frCollator.compare(l, r)

  // Tests whether list contains a given character ignoring character case and accents.
  extension (letters: List[Char]) @tailrec def containsMatchable(letter: Char): Boolean = {
    letters match {
      case Nil => false
      case head :: _ if head == letter => true
      case head :: _ if head =:= letter => true
      case _ :: tail => tail.containsMatchable(letter)
    }
  }

  extension (letters: Set[Char]) def containsMatchable(letter: Char): Boolean = {
    if letters.contains(letter) then true else letters.toList.containsMatchable(letter)
  }
}


///
// State case classes and associated routines.
///

object State {
  import Helpers.*

  case class Words(hangWord: WordEntry, words: List[WordEntry]) {
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
    import Status.*, Guess.applyGuess
    def nextGuess: Char => (Status, Guess) = { letter =>
      if guessSet.containsMatchable(letter) then (NoChange, this) else {
        val `match` = this.hangList.containsMatchable(letter)
        val newGuess = this.copy(
          guessList = if `match` then letter.applyGuess(this.hangList, this.guessList) else this.guessList,
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
    extension (letter: Char) def applyGuess(hangList: List[Char], guessList: List[Char]): List[Char] = {
      guessList.zip(hangList).map((g, h) => if (letter =:= h) h else g)
    }

    extension (letters: List[Char]) private def prefillNons(hangList: List[Char], guessList: List[Char]): List[Char] = {
      letters match {
        case Nil => guessList
        case letter::rest => rest.prefillNons(hangList, letter.applyGuess(hangList, guessList))
      }
    }

    private val whitespace  = List('\u0009', '\u0020')
    private val ligatures   = List('\u0153', '\u00E6')
    private val apostrophes = List('\u0027', '\u2019')
    private val separators  = List(',', '.')
    private val prefills = whitespace ++ ligatures ++ apostrophes ++ separators
    val nextRound: Words => Guess = words => {
      val hangList = words.hangWord.word.splitWord
      val guessList = prefills.prefillNons(hangList, List.fill[Char](hangList.length)('_'))
      Guess(
        hangList = hangList,
        guessList = guessList,
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

  @tailrec def enterGameR(words: Words, winsAndLosses: WinsAndLosses): Unit = {
    words.hangWord.definition.fold(())(definition => println(s"""\tUn indice -> "$definition""""))
    @tailrec def playGameR(guess: Guess): (Boolean, WinsAndLosses) = {
      readLine(fmtinput.format(guess.guessList.joinWord, guess.guesses)) match {
        case s: String if s.isEmpty => playGameR(guess)
        case s: String if s =:= "New" => (false, guess.finishGuess(winsAndLosses))
        case s: String if s =:= "Exit" => (true, guess.finishGuess(winsAndLosses))
        case entry: String => {
          val (cnt, letter) = (1, entry.head)
//        val (cnt, letter) = entry match {
//          case oe if oe =:= "oe" => (2, '\u0153')
//          case ae if ae =:= "ae" => (2, '\u00E6')
//          case entry => (1, entry.head)
//        }
          if ((cnt < entry.length) || !frenchSet.contains(letter.toLower)) {
            println(s"\tSorry, not a valid entry -> $entry")
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
    if !fDone then enterGameR(words.nextWord, newWinsAndLosses)
  }

  // List of words to guess from.
  val fname = if args.isEmpty then "src/resources/dictionaryWords.txt" else args(0)
  val words = Words(WordEntry("HANGMAN", None), Helpers.wordList(fname)).nextWord

  println("Welcome to the Hangman word guessing game.")
  println("Type 'Exit' to leave the game, 'New' for a new game.")
//println("Type 'oe' for \u0153.  Type 'ae' for \u00E6.")
  println("Good luck!\n")
  enterGameR(words, WinsAndLosses())
  println("\nThank you for playing Scala Hangman!")
}