/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Prg 01 - Lexeme (a symbol with a token value)
 */

class Lexeme(var label: String, var token: Token.Value = null) {
  override def toString: String = {
    var s = s"Lexeme($label"
    if (token != null)
      s += s", $token"
    s + ")"
  }
}

object Lexeme {
  def main(args: Array[String]): Unit = {
    val lex = new Lexeme("statement", Token.IDENTIFIER)
    println(lex)
  }
}