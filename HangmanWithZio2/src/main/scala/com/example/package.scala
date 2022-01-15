package com 

package object example {
  sealed abstract case class Guess private (letter: Char)
  object Guess {
    def make(guess: String): Option[Guess] = 
      Some(guess.toList) collect {
        case c :: Nil if c.isLetter => new Guess(c.toLower) {}
      }
  }

  sealed abstract case class Name private (name: String)
  object Name {
    def make(name: String): Option[Name] = 
      if (name.length > 0)
        Some(new Name(name) {})
      else
        None
  }

  sealed abstract case class Word private (word: String) {
    def contains(a: Char): Boolean = word.contains(a)
    def length: Int                = word.length
    def toList: List[Char]         = word.toList
    def toSet: Set[Char]           = word.toSet
  }
  object Word {
    def make(word: String): Option[Word] = 
      if (word.length > 0 && word.forall(_.isLetter))
        Some(new Word(word.toLowerCase) {})
      else
        None
  }

  sealed abstract case class State private (word: Word, guesses: Set[Guess], name: Name) {
    def failCount: Int    = (guesses.map(_.letter) -- word.toSet).size
    def lost: Boolean     = failCount > 5
    def won: Boolean      = (word.toSet -- guesses.map(_.letter)).isEmpty
    def addGuess(g: Guess)= new State(word, guesses + g, name) {}
  }
  object State {
    def initialState(w: Word, n: Name): State = new State(w, Set(), n) {}
  }

  sealed trait GuessResult 
  object GuessResult {
    object Won extends GuessResult
    object Lost extends GuessResult
    object CorrectGuess extends GuessResult
    object BadGuess extends GuessResult
    object InvalidGuess extends GuessResult
  }

  val words = List("hey","boat")

  val hangman = List(
    """
  #  -----
  #  |   |
  #  |
  #  |
  #  |
  #  |
  #  ______
  #""".stripMargin('#'),
  """
  #  -----
  #  |   |
  #  |   O
  #  |
  #  |
  #  |
  #  ______
  #""".stripMargin('#'),
  """
  #  -----
  #  |   |
  #  |   O
  #  |   |
  #  |
  #  |
  #  ______
  #""".stripMargin('#'),
  """
  #  -----
  #  |   |
  #  |   O
  #  |   |
  #  |
  #  |
  #  ______
  #""".stripMargin('#'),
  """
  #  -----
  #  |   |
  #  |   O
  #  |  /|
  #  |
  #  |
  #  ______
  #""".stripMargin('#'),
  """
  #  -----
  #  |   |
  #  |   O
  #  |  /|\
  #  |
  #  |
  #  ______
  #""".stripMargin('#'),
  """
  #  -----
  #  |   |
  #  |   O
  #  |  /|\
  #  |  /
  #  |
  #  ______
  #""".stripMargin('#'),
  """
  #  -----
  #  |   |
  #  |   O
  #  |  /|\
  #  |  / \
  #  |
  #  ______
  #""".stripMargin('#'))
}


