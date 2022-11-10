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
  def boardToString(board: Array[Array[Int]]): String = {
    val sb = new mutable.StringBuilder()
    for (x <- board) {
      x.foreach(y => sb.append(s"$y\t"))
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

  /** Returns true if the sequence is valid, that is, it contains 9 numbers in [0-9] with optionally repeating zeros. */
  def isValid(seq: Array[Int]): Boolean = {
    val pattern = raw"[0-9]{9}".r
    seq.mkString match {
      case pattern(_*) => true
      case _ => false
    }
  }

  // TODO #7: return whether all rows of the given board are valid sequences
  def allRowsValid(board: Array[Array[Int]]): Boolean = false

  // TODO #8: return whether all columns of the given board are valid sequences
  def allColsValid(board: Array[Array[Int]]): Boolean = false

  // TODO #9: return whether all boxes of the given board are valid sequences
  def allBoxesValid(board: Array[Array[Int]]): Boolean = false

  // TODO #10: a board is valid if all of its rows, columns, and boxes are also valid
  def isValid(board: Array[Array[Int]]): Boolean = false

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

  // TODO #12: a board is solved if is complete and valid
  def isSolved(board: Array[Array[Int]]): Boolean = false

  // TODO #13: return a new board configuration from the given one by setting a digit at a specific (row, col) location
  def getChoice(board: Array[Array[Int]], row: Int, col: Int, d: Int): Array[Array[Int]] = null

  // TODO #14: return all possible new board configurations from the given one
  def getChoices(board: Array[Array[Int]]): IndexedSeq[Array[Array[Int]]] = null

  // TODO #15: return a solution to the puzzle (null if there is no solution)
  def solve(board: Array[Array[Int]]): Array[Array[Int]] = null

  def formatStringFromArray[A](arr: Array[A]): String = {
    arr.mkString("[", ", ", "]")
  }

  def main(args: Array[String]): Unit = {

    val boardInputFile1 = "sudoku1.txt"
    val boardInputFile2 = "sudoku2.txt"
    val boardInputFile3 = "sudoku3.txt"

    val board1 = readBoard(boardInputFile1)
    val board2 = readBoard(boardInputFile2)
    val board3 = readBoard(boardInputFile3)

    val boardString1 = boardToString(board1)
    val boardString2 = boardToString(board2)
    val boardString3 = boardToString(board3)

    println(boardString1)
    println("Print columns as sequences:")

    for (i <- board1.indices) println(formatStringFromArray(getCol(board1, i)))

    println(s"isComplete(board1): ${isComplete(board1)}")
    println(s"isComplete(board2): ${isComplete(board2)}")
    println(s"isComplete(board3): ${isComplete(board3)}")

    // val sol = solve(board)
    // println(sol)
  }
}
