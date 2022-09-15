/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Homework 04 - Token
 * Student Name:
 */

object Token extends Enumeration {
  val EOF         = Value
  val CLASS       = Value
  val IDENTIFIER  = Value
  val PUBLIC      = Value
  val ABSTRACT    = Value
  val FINAL       = Value
  val EXTENDS     = Value
  val IMPLEMENTS  = Value
  val BLOCK_OPEN  = Value
  val BLOCK_CLOSE = Value
  val COMMA       = Value
}
