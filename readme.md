# Scala Hangman #

----------
The Hangman word-guessing game hits a nice sweet spot when learning a new computer language. Not as trivial as "Hello World" but not overly difficult to implement.

This version of Hangman was written utilizing the [Scala](https://www.scala-lang.org/index.html "https://www.scala-lang.org/index.html") language, currently at version 3.3.4.     

In this project, [SBT or *The simple Scala build tool* ](http://www.scala-sbt.org/ "http://www.scala-sbt.org/") was utilized to compile Hangman. To build Hangman, use this command:

    sbt.bat compile

To run Hangman, invoke the following command:

    sbt.bat run

Important: Use SBT 1.10.2 or later. An alternative to using the above run command: 
 
	sbt.bat package
	scala.bat -cp target\scala-3.2.2\hangman_3-1.0.0.jar Hangman

The program is text based as shown by:

![console view](https://github.com/ROpsal/scala-hangman/blob/master/images/console.png)