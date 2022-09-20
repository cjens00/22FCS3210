/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Description: Prg 01 - Token
 */

object Token extends Enumeration {
  val EOF             = Value
  val EO_PRG          = Value // $$
  val COMMENT         = Value // ;
  val INPUT           = Value // ?
  val OUTPUT          = Value // !
  val STRING          = Value
  val IDENTIFIER      = Value
  val ASSIGNMENT      = Value // =
  val LITERAL         = Value
  val ADDITION        = Value // +
  val SUBTRACTION     = Value // -
  val MULTIPLICATION  = Value // *
  val DIVISION        = Value // /
  val MODULUS         = Value // %
  val LESS            = Value // <
  val LESS_EQUAL      = Value // <=
  val GREATER         = Value // >
  val GREATER_EQUAL   = Value // >=
  val EQUAL           = Value // ==
  val DIFFERENT       = Value // !=
  val BREAK           = Value // ^
  val DOT             = Value // .
  val OPEN_PAR        = Value // (
  val CLOSE_PAR       = Value // )
  val OPEN_BRACKET    = Value // [
  val CLOSE_BRACKET   = Value // ]
}