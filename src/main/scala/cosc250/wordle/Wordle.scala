package cosc250.wordle

// If we've removed letters from our string, we want still to be able to track their original locations
type CharLocations = Seq[(Char, Int)]

enum Color(val escape: String):
  case Green extends Color("\u001b[32m")
  case Orange extends Color("\u001b[33m")
  case Gray  extends Color("\u001b[38;5;246m")

def greenLetters(target:String, guess:String):Seq[(Char, Int)] = 
  val zipped = target.zip(guess)
  for 
    ((targetLetter, guessLetter), i) <- zipped.zipWithIndex if targetLetter == guessLetter
  yield (targetLetter, i)

def orangeLetters(target:CharLocations, guess:CharLocations):Seq[(Char, Int)] = 
  val targetGroups = target.groupBy(_._1)
  val guessGroups = guess.groupBy(_._1)

  for 
    (c, occurrances) <- guessGroups.toSeq if targetGroups.contains(c)
    (_, index) <- occurrances.zip(guessGroups(c)).map(_._1)
  yield
    c -> index

def colourise(target:String, guess:String) =
  val green = greenLetters(target, guess)
  
  val remainingInTarget = target.zipWithIndex.filterNot(green.contains(_))
  val remainingInGuess = guess.zipWithIndex.filterNot(green.contains(_))
  val orange = orangeLetters(remainingInTarget, remainingInGuess)

  val grey = remainingInGuess.filterNot(orange.contains(_))

  (
    green.map({ case (c, i) => (Color.Green, c, i)}) 
    ++ orange.map({ case (c, i) => (Color.Orange, c, i)})
    ++ grey.map({ case (c, i) => (Color.Gray, c, i)})
  )

def inOrder(triple:Seq[(Color, Char, Int)]):Seq[(Color, Char)] =
  for (col, char, i) <- triple.sortBy(_._3) yield (col, char)

def colourisedString(chars:Seq[(Color, Char)]):String = 
  (for (col, c) <- chars yield s"${col.escape} $c\u001b[0m").mkString


class IO[A](action: () => A):
  def map[B](f: A => B):IO[B] = IO(() => f(action()))
  def flatMap[B](f: A => IO[B]) = f(action())

  def unsafeRunSync:A = action()


def readFile(path:String):IO[Seq[String]] = IO(() => 
  import scala.io.*
  Source.fromFile(path).getLines.toSeq
)
  
def getWordList:IO[Seq[String]] = readFile("src/main/resources/words.txt")

def chooseWord(wordList:Seq[String]):IO[String] = IO(() => 
  import scala.util.Random
  wordList(Random.nextInt(wordList.length))
)

def readGuess(wordList:Seq[String]):IO[String] = IO({ () => 
  var guess = ""
  while {
    println("Enter your guess") 
    guess = scala.io.StdIn.readLine
    !wordList.contains(guess)
  } do {
    println("Not a valid word.")
  }

  guess
})

def printResult(result:Seq[(Color, Char)]):IO[Unit] = IO(() =>
  println(colourisedString(result))
)

case class GameState(wordList:Seq[String], target:String, guessesRemaining:Int):
  def doGuess:IO[GameState] = 
    for 
      _ <- IO(() => println(s"You have $guessesRemaining guesses remaining."))
      guess <- readGuess(wordList)
      result = inOrder(colourise(target,guess))
      _ <- printResult(result)
    yield
      GameState(wordList, target, guessesRemaining - 1)

  def play:IO[Unit] =
    if guessesRemaining <= 0 then IO(() => println(target)) else doGuess.flatMap(_.play)

@main def testread = 
  println("Enter")
  val x = scala.io.StdIn.readLine
  println(x)

@main def wordle = 
  (for 
    wordList <- getWordList
    target <- chooseWord(wordList)
    gs = GameState(wordList, target, 6)
    _ <- gs.play
  yield ()).unsafeRunSync


@main def run =
  println(colourisedString(inOrder(colourise("SCALA", "SCRAM"))))

@main def five = 
  import scala.io.*

  val file = Source.fromFile("src/main/resources/words.txt").getLines
  for line <- file if line.length == 5 && !line.contains('-') do println(line)
