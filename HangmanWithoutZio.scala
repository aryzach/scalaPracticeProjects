import scala.annotation.tailrec
import scala.io.StdIn

case class Rnd(seed:Int) {
  def nextInt(bound: Int):(Int, Rnd) = {
    val rng = new scala.util.Random(seed)
    val res = rng.nextInt(bound-1)
    val newSeed = Rnd(seed+1)
    (res, newSeed)
  } 
}

case class State(wordsGuessed: Seq[String], lettersGuessed: Seq[Char], word: String, rand: Rnd)
trait Guess 
case class Word(guess: String) extends Guess 
case class Letter(guess: Char) extends Guess 


object Hangman extends App {
  val rand = Rnd(11111112) 
  val words = List("HEY","BOAT","A","AB")
  val initialState = initializeGame(words, State(Seq(), Seq(), "", rand))
  main(initialState)


  def main(s: State): Unit = {
    println(s)
    val winOrLose = playGame(s)
    

    println(winOrLose)
    println("more?")

    //val userInput = StdIn.readLine().trim


    //if (userInput == "y") 
    main(initializeGame(words, s))
  }

  def gameOver(s: State): Boolean = s.wordsGuessed.contains(s.word) || s.word.foldLeft(true)((b,x) => s.lettersGuessed.contains(x) && b)
  def endGame(s: State): Unit = println("done")

  // word already picked
  def playGame(s: State): Boolean = {
    if (gameOver(s))
      endGame(s)
    else
      playGame(handleValidGuess(s)(playTurn(s)))
    true
  }

  def handleValidGuess(s: State)(g: Guess): State =
    // update based on valid turn response
    g match {
      case Word(w)   => handleValidWord(w)(s)
      case Letter(l) => handleValidLetter(l)(s)
    }

  def handleValidLetter(l: Char)(s:State): State = 
      s.copy(lettersGuessed = l +: s.lettersGuessed) 

  def handleValidWord(w: String)(s:State): State = 
      s.copy(wordsGuessed = w +: s.wordsGuessed) 

  def initializeGame(words: List[String], s: State): State = {
    val (i, r) = s.rand.nextInt(words.length)
    println(words.length)
    println(i)
    State(Seq(), Seq(), words(i), r)
  }


  def promptTurn(s: State): Unit = {
    println("word is:")
    println(displayWord(s.word, s.lettersGuessed))
    println("wrong guesses:")
    println(displayWrong(s.word, s.lettersGuessed))
    val avail = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" filter (x => !s.lettersGuessed.contains(x))
    println(s"available letters $avail")
    println("enter letter: ")
  }

  def getInput(): String = StdIn.readLine().toUpperCase

  def displayWrong(word: String, lettersGuessed: Seq[Char]): Seq[Char] = 
    lettersGuessed.filter(x => !word.contains(x))

  def displayWord(word: String, lettersGuessed: Seq[Char]): String = 
    word.map(x => if (lettersGuessed.contains(x)) s"$x " else "_ ").mkString("")

  def validateLetterGuess(s: State, g: Char): Guess = 
    if (s.lettersGuessed contains g) {
      println("already guessed that")
      playTurn(s)
    } else
      Letter(g)

  // this where to check if same length and placeholders match
  def validWordGuess(s: State, g: String): Boolean = true

  def validateWordGuess(s: State, g: String): Guess = 
    if (validWordGuess(s, g))
      Word(g)
    else {
      println("not a valid word guess")
      playTurn(s)
    }

  def handleNoInput(s: State) = playTurn(s)

  //def validateGuess(s: State, g: String, f: String => Boolean): String 
  // make a curried partially applied function, f, that takes state and a validating function like validGuess or s.lettersGuessed contains g

  def getValidResponse(s: State): Guess = {
    val resp = getInput()
    if (resp.length > 1) {
      validateWordGuess(s, resp)
    } else if (resp.length == 1) {
      validateLetterGuess(s, resp.head)
    } else {
      handleNoInput(s)
    }
  }

  def playTurn(s: State): Guess = {
    promptTurn(s)
    getValidResponse(s)
  }
}
  







