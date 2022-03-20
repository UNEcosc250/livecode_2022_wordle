package cosc250.wordle


// A homegrown synchronous effect class.
class IO[A](action: () => A):
  def map[B](f: A => B):IO[B] = IO(() => f(action()))
  def flatMap[B](f: A => IO[B]) = f(action())

  // Runs the program
  def unsafeRunSync:A = action()

// An IO effect for reading a list of strings from a text file
def readFile(path:String):IO[Seq[String]] = IO(() => 
  import scala.io.*
  Source.fromFile(path).getLines.toSeq
)


def getWordList:IO[Seq[String]] = readFile("src/main/resources/words.txt")

// Choosing a random word also requires "IO" in that it takes in a random number from the environment
def chooseWord(wordList:Seq[String]):IO[String] = IO(() => 
  import scala.util.Random
  wordList(Random.nextInt(wordList.length))
)

// Here, we've taken some locally mutable code and wrapped it in an IO effect, so it's an effect not a side-effect
def readGuess(wordList:Seq[String]):IO[String] = IO({ () => 
  var guess = ""
  while {
    println("Enter your guess") 
    guess = scala.io.StdIn.readLine.toUpperCase
    !wordList.contains(guess)
  } do {
    println("Not a valid word.")
  }

  guess
})

// printing output as an IO effect
def printResult(result:Seq[(Color, Char)]):IO[Unit] = IO(() =>
  println(colourisedString(result))
)
