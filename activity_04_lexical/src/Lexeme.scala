/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Activity 04 - Lexeme: a symbol with an optional token value
 * Student(s) Name(s):
 */

class Lexeme(var label: String, var token: Token.Value = null) {
  override def toString: String = {
    var s = s"Lexeme($label"
    if (token != null)
      s += s", $token"
    s + ")"
  }
}

