package cosc250.wordlelive

class IO[A](val action: () => A) {

  def map[B](f: A => B):IO[B] = 
    IO(() => f(action()))

  def flatMap[B](f: A => IO[B]):IO[B] = 
    IO(() => f(action()).action())

  def unsafeRunSync():A = action()
}

def readFile(path:String):IO[Seq[String]] = {
  import scala.io.*
  IO(() => Source.fromFile(path).getLines.toSeq)
}

def readWords = readFile("src/main/resources/words.txt")

def chooseWord(wordList:Seq[String]):IO[String] =
  import scala.util.Random
  IO(() => wordList(Random.nextInt(wordList.length)))


def readGuess(wordList:Seq[String]):IO[String] = IO(() => 
  var guess = ""
  while {
    println("Enter a guess")
    guess = scala.io.StdIn.readLine.toUpperCase
    !wordList.contains(guess)
  } do {
    println("Not a valid word")
  }

  guess
)

@main def testIO = 
  val gameIO:IO[Unit] = 
    for 
      words <- readWords
      chosen <- chooseWord(words)
      done <- GameState(words, chosen, 6).play
    yield done

  println(gameIO.unsafeRunSync())
  
  

