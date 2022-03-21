package cosc250.wordlelive

type CharLocations = Seq[(Char, Int)]

def greenLetters(target:String, guess:String):Seq[(Char, Int)] =
  for 
    ((a, b), i) <- target.zip(guess).zipWithIndex if a == b
  yield (a, i)

def orangeLetters(target:CharLocations, guess:CharLocations) = 
  val targetGroups = target.groupBy({ case (c, _) => c})
  val guessGroups = guess.groupBy({ case (c, i) => c})

  for 
    (c, locations) <- guessGroups.toSeq if targetGroups.contains(c)
    targetLocations = targetGroups(c)
    ((cc, i), _) <- locations.zip(targetLocations)
  yield 
    (cc, i)


enum Colour(val code: String):
  case Green extends Colour("\u001b[32m")
  case Orange extends Colour("\u001b[33m")
  case Grey extends Colour("\u001b[38;5;248m")

def colourise(target:String, guess:String):Seq[(Char, Colour)] = 
  val green = greenLetters(target, guess)
  val remainingTarget:CharLocations = 
    target.zipWithIndex.filterNot(green.contains(_))
  val remainingGuess:CharLocations = 
    guess.zipWithIndex.filterNot(green.contains(_))
  val orange = orangeLetters(remainingTarget, remainingGuess)
  val grey = remainingGuess.filterNot(orange.contains(_))

  val triples = 
    (for (c, i) <- green yield (Colour.Green, c, i))
    ++ (for (c, i) <- orange yield (Colour.Orange, c, i))
    ++ (for (c, i) <- grey yield (Colour.Grey, c, i))

  for (col, char, _) <- triples.sortBy(_._3) yield (char, col)


def terminalString(seq:Seq[(Char, Colour)]):String = 
  (for 
    (char, col) <- seq
  yield s"${col.code} $char ").mkString

@main def run =
  val target = "SCALA"
  val guess = "SCRAM"
  
  println(terminalString(colourise(target, guess)))