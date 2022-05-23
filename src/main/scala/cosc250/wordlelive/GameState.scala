package cosc250.wordlelive

case class GameState(wordList:Seq[String], target:String, guessesRemaining:Int) {

  def doGuess:IO[GameState] = {
    for {
      _ <- IO(() => println(s"You have $guessesRemaining guesses remaining"))
      guess <- readGuess(wordList)
      result = terminalString(colourise(target, guess))
      _ <- IO(() => println(result))
    } yield {
      GameState(wordList, target, guessesRemaining - 1)
    }
  }

  // recursive!
  def play:IO[Unit] = 
    if (guessesRemaining <= 0) then 
      IO(() => println(target))
    else
      doGuess.flatMap(_.play)

}