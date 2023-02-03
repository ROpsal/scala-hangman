/**
 * Author: Richard B. Opsal, Ph.D.
 * Update: 2015/08/21
**/

///
// Helper functions for Hangman application.
///

package object hm {

  // Generate list of possible words from passed file.
  def wordList(fname : String = "dictionaryWords.txt") : List[String] = {
    val source = io.Source.fromFile( fname )
    val words : List[String] = source.getLines.toList
    source.close()
    words
  }

  // Return a random word from the passed list.
  def randomWord(words : List[String]) : String = {
    words( scala.util.Random.nextInt(words.length) )
  }

  // Split the word into individual letters.
  def wordSplit(word : String) : List[Char] = {
    word.toList
  }

  // Join the list of characters together with a space in-between.
  def wordJoin(wordlist : List[Char]) : String = {
    wordlist.mkString(" ")
  }

  // Set of upper case letters.
  def alphaSet : Set[Char] = {
    ('A' to 'Z').toSet
  }

  // Generate a new guess list based on letter, current matches and actual word.
  def applyGuess(letter : Char, guesslist : List[Char], hanglist : List[Char]) : List[Char] = {
    guesslist.zip(hanglist).map({ case (g, h) => if (letter == h) h else g })
  }
}

///
// The Hangman application.
///

@main def Hangman(args: String*): Unit = {

  // Cheery intro to the Hangman game.
  println("Welcome to the Hangman word guessing game.")

  // List of words to guess from.
  val fname = if (args.isEmpty) "src/resources/dictionaryWords.txt" else args(0)
  var words = hm.wordList(fname)

  // Allowable set of characters to guess from.
  val alphaset : Set[Char] = hm.alphaSet

  // Formatting strings for nice output.
  val fmtsummary = "%s  Wins : %2d  Losses : %2d"
  val fmtinput   = "\t%s  [Guesses left : %2d ] Letter : "

  // The game play loop.
  var fexit = false
  var wins   = 0
  var losses = 0
  while (!fexit) {
    println("Type 'Exit' to leave the game, 'New' for a new game.")
    println("Good luck!\n")

    val hangword = hm.randomWord(words)
    words = words.filterNot(_ == hangword)
    val hanglist = hm.wordSplit(hangword.toUpperCase)

    var guesslist = hm.wordSplit("_" * hangword.length)
    var guesset : Set[Char] = Set()
    var guesses = 6

    var fnewgame = false
    while (!fnewgame && !fexit) {

      def checkGuess() : Unit = {

        def printSummary(message : String) : Unit = {
          println("\t" + hm.wordJoin(hanglist) + "\n")
          println(fmtsummary.format(message, wins, losses))
        }

        if (hanglist == guesslist) {
          fnewgame = true
          wins += 1
          printSummary("Congratulations on your win!")

        } else {
          guesses match {
            case 1 =>
              fnewgame = true
              losses += 1
              printSummary("Too Bad! Please try again.")
            case _ => guesses -= 1
          }
        }
      }

      val line = scala.io.StdIn.readLine(fmtinput.format(hm.wordJoin(guesslist), guesses)).toUpperCase
//    println(line)
      line match {
        case "NEW"  => guesses = 1 ; checkGuess()
        case "EXIT" => fexit = true
        case "" => Nil
        case _ => {
          val letter : List[Char] = line.toList
          if ((1 < letter.length) || !alphaset.contains(letter.head)) {
            // An unexpected entry.
            println("Not a valid guess -> " + line)

          } else if (guesset.contains(letter.head)) {
            // Ignore this case. Already guessed this letter.

          } else {
            if (hanglist.contains(letter.head)) {
              guesslist = hm.applyGuess(letter.head, guesslist, hanglist)
              guesses += 1
            }

            guesset = guesset + letter.head
            checkGuess()
          }
        }
      }
    }
  }

  println("Thank you for playing Scala Hangman!")
}
