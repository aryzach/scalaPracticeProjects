package com.example
import zio._
import zio.console._
import java.io.IOException
import zio.random._

object Hangman extends App {
  def getInput(prompt: String): ZIO[Console, IOException, String] =
    putStrLn(prompt).flatMap(_ => getStrLn) 

  lazy val getName: ZIO[Console, Nothing, Name] = {
    for {
      input <- getInput("what's your name?")
      name <- ZIO.fromOption(Name.make(input)) <> putStrLn("invalid input").flatMap(_ => getName)
    } yield name
  }

  lazy val getWord: ZIO[Random, Nothing, Word] = {
    for {
      i <- nextIntBounded(words.length)
      w <- ZIO.fromOption(
        for {
          b <- words.lift(i)
          z <- Word.make(b)
        } yield z 
        ).orDieWith(_ => new Error("index out of bounds?"))
    } yield w
  }

  def getMove(): ZIO[Console, IOException, Guess] = {
    for {
      input <- getInput("make a guess")
      guess <- ZIO.fromOption(Guess.make(input)) <> putStrLn("invalid input") *> getMove
    } yield guess 
  }


  def showState(s: State): ZIO[Console, Nothing, Unit] = {
    val showHangman = hangman(s.failCount)
    val showWord    = s.word.toList.map(x => if (s.guesses.map(_.letter).contains(x)) s" $x " else '_').mkString
    val showGuess   = (s.guesses.map(_.letter) -- s.word.toSet).mkString(" ")
    putStrLn(s"""
      #$showHangman
      #$showWord
      #$showGuess
      #""".stripMargin('#')
      )
  }

  def analyzeGuess(s: State)(ns: State)(g: Guess): GuessResult = {
    if (s.guesses.contains(g)) GuessResult.InvalidGuess
    else if (ns.won) GuessResult.Won
    else if (ns.lost) GuessResult.Lost
    else if (ns.failCount > s.failCount) GuessResult.BadGuess
    else GuessResult.CorrectGuess
  }


  def mainLoop(s: State): ZIO[Console, IOException, Unit] = 
    for {
      g <- showState(s) *> getMove() 
      ns = s.addGuess(g)
      gr = analyzeGuess(s)(ns)(g) 
      _ <- gr match {
        case GuessResult.CorrectGuess => putStrLn("nice!") *> mainLoop(ns)
        case GuessResult.Won => putStrLn("you won!")
        case GuessResult.Lost => putStrLn("you lost!")
        case GuessResult.InvalidGuess => putStrLn("invalid!") *> mainLoop(ns)
        case GuessResult.BadGuess => putStrLn("bad guess!") *> mainLoop(ns)
      }
    } yield ()

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = 
    (for {
      n <- getName
      w <- getWord
      _ <- mainLoop(State.initialState(w, n))
    } yield ()).exitCode


}
