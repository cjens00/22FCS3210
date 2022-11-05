/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Prg 02 - Sudoku Puzzle
 */

import scala.io._

object Sudoku {

  // TODO #1: return an 2D array of Int representing a sudoku board given a filename
  def readBoard(fileName: String): Array[Array[Int]] = {
    null
  }

  // TODO #2: return a String representation from a given sudoku board
  def boardToString(board: Array[Array[Int]]): String = ""

  // TODO #3: return a specific row from a sudoku board as a sequence of numbers
  def getRow(board: Array[Array[Int]], row: Int): Array[Int] = null

  // TODO #4: return a specific column from a sudoku board as a sequence of numbers
  def getCol(board: Array[Array[Int]], col: Int): Array[Int] = null

  // TODO #5: return a specific box from a sudoku board as a sequence of numbers
  def getBox(board: Array[Array[Int]], x: Int, y: Int): Array[Int] = null

  // TODO #6: a sequence is valid if it has 9 numbers in [0-9] with possibly repeated zeros
  def isValid(seq: Array[Int]): Boolean = false

  // TODO #7: return whether all rows of the given board are valid sequences
  def allRowsValid(board: Array[Array[Int]]): Boolean = false

  // TODO #8: return whether all columns of the given board are valid sequences
  def allColsValid(board: Array[Array[Int]]): Boolean = false

  // TODO #9: return whether all boxes of the given board are valid sequences
  def allBoxesValid(board: Array[Array[Int]]): Boolean = false

  // TODO #10: a board is valid if all of its rows, columns, and boxes are also valid
  def isValid(board: Array[Array[Int]]): Boolean = false

  // TODO #11: a board is complete it there is no zero
  def isComplete(board: Array[Array[Int]]): Boolean = false

  // TODO #12: a board is solved if is complete and valid
  def isSolved(board: Array[Array[Int]]): Boolean = false

  // TODO #13: return a new board configuration from the given one by setting a digit at a specific (row, col) location
  def getChoice(board: Array[Array[Int]], row: Int, col: Int, d: Int): Array[Array[Int]] = null

  // TODO #14: return all possible new board configurations from the given one
  def getChoices(board: Array[Array[Int]]): IndexedSeq[Array[Array[Int]]] = null

  // TODO #15: return a solution to the puzzle (null if there is no solution)
  def solve(board: Array[Array[Int]]): Array[Array[Int]] = null

  def main(args: Array[String]): Unit = {
    val board = readBoard("sudoku1.txt")
    val sol = solve(board)
    println(boardToString(sol))
  }
}
