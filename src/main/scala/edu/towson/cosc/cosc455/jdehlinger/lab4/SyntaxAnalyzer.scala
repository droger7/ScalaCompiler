package edu.towson.cosc.cosc455.jdehlinger.lab4


class SyntaxAnalyzer {

  // For ease, store the terminal literals in a List
  val ARTICLES : List[String] = List("teh", "a")
  val VERBS : List[String] = List("ates", "lovez", "hatez")
  val NOUNS : List[String] = List("kat", "dawg", "rat")
  val ADJECTIVES : List[String] = List("fat", "happy", "hungry", "mean")
  val ADVERBS : List[String] = List("accidently", "quickly", "secretly", "empty")

  // Flag for errors and helper methods
  var errorFound : Boolean = false
  def setError() = errorFound = true
  def resetError() = errorFound = false
  def getError : Boolean = errorFound


  // This method implements the BNF rule for a sentence <S> ::= <NP> <V> <NP>
  def Sentence() = {
    resetError()
    if(!errorFound) NounPhrase()
    if(!errorFound) Adverb()
    if(!errorFound) Verb()
    if(!errorFound) NounPhrase()
  }

  // This method implements the BNF rule for a noun phrase <NP> ::= <A> <N>
  def NounPhrase() = {
    if(!errorFound) Article()
    if(!errorFound) Adjective()
    if(!errorFound) Noun()
  }

  // This method implements the BNF rule for a verb <V> ::= ates | hatez | hatez
  def Verb() = {
    if (VERBS contains Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    else {
      println("SYNTAX ERROR - A verb was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  // This method implements the BNF rule for a noun <N> ::= dawg | kat | rat
  def Noun() = {
    if (NOUNS contains Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    else {
        println("SYNTAX ERROR - A noun was expected when '" + Compiler.currentToken + "' was found.")
        setError()
      }
  }

  // This method implements the BNF rule for an article <N> ::= teh | a
  def Article() = {
    if (ARTICLES contains Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    else {
        println("SYNTAX ERROR - An article was expected when '" + Compiler.currentToken + "' was found.")
        setError()
      }
  }
  def Adjective() = {
    if (ADJECTIVES contains Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    else {
      println("SYNTAX ERROR - An adjective was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }
  def Adverb() = {
    if (ADJECTIVES contains Compiler.currentToken)
      Compiler.Scanner.getNextToken()

  }
}
