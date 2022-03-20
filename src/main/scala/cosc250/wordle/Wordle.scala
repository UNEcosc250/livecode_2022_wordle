package cosc250.wordle

// If we've removed letters from our string, we want still to be able to track their original locations
type CharLocations = Seq[(Char, Int)]

enum Color(val escape: String):
  case Green extends Color("\u001b[32m")
  case Orange extends Color("\u001b[33m")
  case Gray  extends Color("\u001b[38;5;246m")

// Given a target word and a guess, what letters would be marked in green?
def greenLetters(target:String, guess:String):Seq[(Char, Int)] = 
  val zipped = target.zip(guess)
  for 
    ((targetLetter, guessLetter), i) <- zipped.zipWithIndex if targetLetter == guessLetter
  yield (targetLetter, i)

// Given the non-green letters (and their locations) from the target word and guess, works out
// what would be orange
def orangeLetters(target:CharLocations, guess:CharLocations):Seq[(Char, Int)] = 
  val targetGroups = target.groupBy(_._1)
  val guessGroups = guess.groupBy(_._1)

  for 
    (c, occurrances) <- guessGroups.toSeq if targetGroups.contains(c)
    (_, index) <- occurrances.zip(guessGroups(c)).map(_._1)
  yield
    c -> index

// Does the working for a wordle round - take the target word and the guess and work out what letters
// are in what colour
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

// Our letters and their colours come out in unknown order. For easy printing,
// put them back in order.
def inOrder(triple:Seq[(Color, Char, Int)]):Seq[(Color, Char)] =
  for (col, char, i) <- triple.sortBy(_._3) yield (col, char)

// Turns a sequence of characters and colours into a coloured terminal string
def colourisedString(chars:Seq[(Color, Char)]):String = 
  (for (col, c) <- chars yield s"${col.escape} $c\u001b[0m").mkString

// Decribes the state of a game...
case class GameState(wordList:Seq[String], target:String, guessesRemaining:Int):
  // runs a single guess from this game state to produce a new game state
  def doGuess:IO[GameState] = 
    for 
      _ <- IO(() => println(s"You have $guessesRemaining guesses remaining."))
      guess <- readGuess(wordList)
      result = inOrder(colourise(target,guess))
      _ <- printResult(result)
    yield
      GameState(wordList, target, guessesRemaining - 1)

  // play is recursive.
  def play:IO[Unit] =
    if guessesRemaining <= 0 then IO(() => println(target)) else doGuess.flatMap(_.play)

// We can describe our program in terms of its IO actions
def wordleGame:IO[Unit] = 
  for 
    wordList <- getWordList
    target <- chooseWord(wordList)
    gs = GameState(wordList, target, 6)
    _ <- gs.play
  yield ()

// To execute our program, we run the program description
@main def runWordle = wordleGame.unsafeRunSync
