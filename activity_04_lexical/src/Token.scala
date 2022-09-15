/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Activity 04 - Token
 * Student(s) Name(s):
 */

object Token extends Enumeration {
  val EOF             = Value
  val ADDITION        = Value // +
  val SUBTRACTION     = Value // -
  val MULTIPLICATION  = Value // *
  val DIVISION        = Value // /
  val IDENTIFIER      = Value
  val LITERAL         = Value
}
