/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Student Name: Cameron Jensen
 * Description: Prg 02 - Sudoku Puzzle
 */

import scala.collection.mutable
import scala.io._

object Sudoku {

  /** Returns a 2D array of Int representing a sudoku board given a filename. */
  def readBoard(fileName: String): Array[Array[Int]] = {
    val bufferedSource = Source.fromFile(fileName)
    val boardMtx: Array[Array[Int]] = Array.ofDim(9, 9)
    val validationPattern = "[0-9]{81}".r
    val source = bufferedSource.getLines.mkString.filter(!_.equals(' '))
    source match {
      case validationPattern(_*) =>
        for {
          i <- 0 to 8
          j <- 0 to 8
        } boardMtx(i)(j) = source(9 * i + j).asDigit
      case _ => throw new IllegalArgumentException("Error reading board: invalid format")
    }
    boardMtx
  }

  /** Returns a String representation of a given sudoku board */
  def boardToString(board: Array[Array[Int]]): String = {
    val sb = new mutable.StringBuilder()
    for (x <- board) {
      x.foreach(y => sb.append(s"$y\t"))
      sb.append('\n')
    }
    sb.toString
  }

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
    val inputFile = "sudoku1.txt"
    val board = readBoard(inputFile)
    val sol = solve(board)
    val boardString = boardToString(board)
    println(boardString)
  }
}
