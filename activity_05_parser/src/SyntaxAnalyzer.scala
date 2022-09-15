/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Activity 05 - SyntaxAnalyzer (an iterable syntax analyzer)
 */

/*
expression  = term expression'
expression' = ( ´+´  | ´-´ ) term expression' | epsilon
term        = factor term'
term'       = ( ´*´ | ´/´ ) factor term' | epsilon
factor      = identifier | literal | ´(´ expression ´)´
identifier  = letter { ( letter | digit ) }
letter      = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´w´ | ´x´ | ´y´ | ´z´
literal     = digit { digit }
digit       = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
*/

class SyntaxAnalyzer(private var source: String) {

  private val it = new LexicalAnalyzer(source).iterator
  private var current: Lexeme = null

  // returns the current lexeme
  private def getLexeme: Lexeme = {
    if (current == null)
      nextLexeme
    current
  }

  // advances the input one lexeme
  private def nextLexeme = {
    current = it.next
  }

  // parses the program, returning its corresponding parse tree
  def parse: Node = {
    parseExpr()
  }

  // expression  = term expression'
  private def parseExpr(): Node = {

    // TODO: create a new (tree) node with lexeme-label "expression"

    // TODO: parse a term

    // TODO: parse an expressionPrime

    // TODO: return the (tree) node created
  }

  // term = factor term'
  private def parseTerm(): Node = {

    // TODO: create a new (tree) node with lexeme-label "term"

    // TODO: parse a factor

    // TODO: parse a termPrime

    // TODO: return the (tree) node created
  }

  // factor = identifier | literal | ´(´ expression ´)´
  private def parseFactor(): Node = {

    // TODO: create a new (tree) node with lexeme-label "factor"

    // TODO: decide whether to add identifier, literal or expression in parentheses branches

    // TODO: return the (tree) node created
  }

  // term' = ( ´*´ | ´/´ ) factor term' | epsilon
  def parseTermPrime(): Node = {

    // TODO: create a new (tree) node with lexeme-label "term"

    // TODO: add as many multiplication or division branches followed by a factor;
    // use epsilon production as the base-case

    // TODO: return the (tree) node created
  }

  // expression' = ( ´+´  | ´-´ ) term expression' | epsilon
  def parseExprPrime(): Node = {

    // TODO: create a new (tree) node with lexeme-label "expression"

    // TODO: add as many addition or subtraction branches followed by a term;
    // use epsilon production as the base-case

    // TODO: return the (tree) node created
  }
}

object SyntaxAnalyzer {
  def main(args: Array[String]): Unit = {

    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse
    print(parseTree)
  }
}
