/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Student Name: Cameron Jensen
 * Description: Prg 02 - Sudoku Puzzle
 */

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io._
import scala.util.chaining.scalaUtilChainingOps

object Sudoku {

  /** Returns a 2D array of Int representing a sudoku board given a filename. */
  def readBoard(fileName: String, boardSize: Int = 9): Array[Array[Int]] = {
    val bufferedSource = Source.fromFile(fileName)
    val boardMtx: Array[Array[Int]] = Array.ofDim(boardSize, boardSize)
    val source = bufferedSource.getLines.mkString.filter(!_.equals(' '))
    // Good spot for a test- check that pattern match is working as expected
    val validationPattern = ("[0-9]" + s"{${math.pow(boardSize, 2).toInt}}").r
    source match {
      case validationPattern(_*) =>
        for {
          i <- 0 until boardSize
          j <- 0 until boardSize
        } boardMtx(i)(j) = source(boardSize * i + j).asDigit
      case _ => throw new IllegalArgumentException("Error reading board: invalid format")
    }
    boardMtx
  }

  /** Returns a String representation of a given sudoku board */
  def boardToString(board: Array[Array[Int]], addWhitespace: Boolean = false): String = {
    val sb = new mutable.StringBuilder()
    for (x <- board) {
      if (addWhitespace)
        x.foreach(y => sb.append(s"$y\t"))
      else
        x.foreach(y => sb.append(s"$y"))
      sb.append('\n')
    }
    sb.toString
  }

  /** Returns a specific row from a sudoku board as a sequence of numbers. */
  def getRow(board: Array[Array[Int]], row: Int): Array[Int] = {
    board(row)
  }

  /** Return a specific column from a sudoku board as a sequence of numbers. */
  def getCol(board: Array[Array[Int]], col: Int): Array[Int] = {
    board.transpose.apply(col)
  }

  /** Returns a specific box from a sudoku board as a sequence of numbers. */
  def getBox(board: Array[Array[Int]], x: Int, y: Int): Array[Int] = {
    isSquare(board) match {
      case true =>
        val boxSize = math.sqrt(board.length).toInt
        var box: Array[Array[Int]] = Array.ofDim(boxSize, boxSize)
        box = board.slice(y * boxSize, y * boxSize + boxSize)
        box.foreach(_.slice(x * boxSize, x * boxSize + boxSize))
        box.flatten
      case false => throw new Exception("Error: Expected a square integer matrix.")
      case _ => throw new Exception("Error: Cannot determine squareness of matrix.")
    }
  }

  /** Returns an array containing each box in board's array. */
  def getAllBoxes(board: Array[Array[Int]]): Array[Array[Int]] = {
    val dim = math.sqrt(board.length).toInt
    val boxes: Array[Array[Int]] = Array.ofDim(board.length)
    for {
      x <- 0 until dim
      y <- 0 until dim
    } boxes(x * dim + y) = getBox(board, x, y)
    boxes
  }

  /** Returns true if the sequence is valid, that is, it contains 9 numbers in [0-9] with optionally repeating zeros. */
  def isValid(seq: Array[Int]): Boolean = {
    val patternCheckNumeric = raw"[0-9]{9}".r
    val matchString = seq.mkString
    matchString match {
      case patternCheckNumeric(_*) =>
        val matchSet = matchString.toSet
        if (matchSet.size.equals(9)) true
        else if (matchString.toList.count(_.equals('0')).equals(9 - matchSet.size)) false
        else true
      case _ => false
    }
  }

  /** A board is valid if all of its rows, columns, and boxes are also valid. */
  def isValid(board: Array[Array[Int]]): Boolean = {
    if (allRowsValid(board) &&
      allColsValid(board) &&
      allBoxesValid(board))
      true
    else false
  }

  /** Returns true if all rows of the given board are valid sequences. */
  def allRowsValid(board: Array[Array[Int]]): Boolean = {
    board.foreach { row => if (!isValid(row)) return false }
    true
  }

  /** Returns true if all columns of the given board are valid sequences. */
  def allColsValid(board: Array[Array[Int]]): Boolean = {
    board.indices.foreach { i =>
      if (!isValid(getCol(board, i))) return false
    }
    true
  }

  /** Return whether all boxes of the given board are valid sequences. */
  def allBoxesValid(board: Array[Array[Int]]): Boolean = {
    for {
      x <- 0 until math.sqrt(board.length).toInt
      y <- 0 until math.sqrt(board.length).toInt
    } {
      if (!isValid(getBox(board, x, y))) return false
    }
    true
  }

  /** Returns true if a Sudoku board is of size n x n. */
  def isSquare[A](board: Array[Array[A]]): Boolean = {
    if (board.length.>(0)) board.length == board(0).length
    else throw new IllegalArgumentException("Error: Board not or improperly initialized.")
  }

  /** Returns true if board is complete, that is, it contains no zeros. */
  def isComplete(board: Array[Array[Int]]): Boolean = {
    val pattern = raw"[\s\S]+[0]+[\s\S]+".r
    val flatBoardStr = board.flatten.mkString
    flatBoardStr match {
      case pattern(_*) => false
      case _ => true
    }
  }

  /** True if the board is both complete and valid. */
  def isSolved(board: Array[Array[Int]]): Boolean = {
    if (isComplete(board) && isValid(board)) true
    else false
  }

  /** Return a new board configuration from the given one by setting a digit at a specific (row, col) location. */
  def getChoice(board: Array[Array[Int]], row: Int, col: Int, d: Int): Array[Array[Int]] = {
    val newBoard = board.clone
    newBoard(row)(col) = d
    newBoard
  }

  /** Return all possible new board configurations from the given one. */
  def getChoices(board: Array[Array[Int]]): IndexedSeq[Array[Array[Int]]] = {
    var choices: ListBuffer[Array[Array[Int]]] = ListBuffer()
    for {
      x <- 1 to 9
      y <- 1 to 9
    } {
      val row = getRow(board, x)
      val col = getCol(board, y)
      val box = getBox(board, x, y)
      val validEntries = (1 to 9).filter(v =>
        row.contains(v) || col.contains(v) || box.contains(v))
      validEntries.foreach(choice =>
        choices.append(getChoice(board, x, y, choice)))
    }
    choices.toIndexedSeq
  }

  /** Return a solution to the puzzle (null if there is no solution). */
  def solve(board: Array[Array[Int]]): Array[Array[Int]] = {
    for (choice <- getChoices(board)) if (isSolved(choice)) return choice
    null
  }

  /** Return a string formatted with all elements of array.
   * Note: Original in-line code provided by IntelliJ IDEA. */
  def formatStringFromArray[A](array: Array[A]): String = {
    array.mkString("[", ", ", "]")
  }

  def main(args: Array[String]): Unit = {
    var boardInputFile = ""
    if (args.length.equals(1)) boardInputFile = args(0)
    else boardInputFile = "sudoku1.txt"

    val board = readBoard(boardInputFile)
    val boardString = boardToString(board, addWhitespace = true)
    println(boardString)
    println(allRowsValid(board))
    println(allColsValid(board))
    println(allBoxesValid(board))

    val boardInputFile2 = "sudoku2.txt"
    val board2 = readBoard(boardInputFile2)
    println(allRowsValid(board2))
    println(allColsValid(board2))
    println(allBoxesValid(board2))

    // Original Code:
    // val sol = solve(board)
    // println(sol)
  }
}
