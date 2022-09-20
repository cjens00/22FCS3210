/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Prg 01 - MouseInterpreter
 * Student(s) Name(s):
 */

import scala.collection.mutable
import scala.io.StdIn
import scala.util.control.Breaks.{break, breakable}

class MouseInterpreter(private var parseTree: Node) {

  private val stack        = mutable.Stack[Int]()
  private val memory       = mutable.LinkedHashMap[String, Int]()

  // set this to true to get detailed debug info
  val DEBUG = false

  // displays the state of the stack and the memory for debug purposes
  def displayStackAndMemory = {
    // show stack
    print("Stack: [top] ")
    for (el <- stack)
      print(el + " ")
    println("[bottom]")
    // show memory
    println("Memory: ")
    var address = 0
    for (tuple <- memory) {
      println(s"\t${address}: ${tuple._1}=${tuple._2}")
      address += 1
    }
  }

  // returns the "address" of a given variable (name)
  def addressOf(name: String): Int = {
    if (!memory.contains(name))
      memory(name) = 0
    var address = 0
    for (tuple <- memory) {
      if (tuple._1.equals(name))
        return address
      address += 1
    }
    throw new Error("Runtime Error: memory corrupted!")
  }

  // returns the value of a given variable (name)
  def valueOf(name: String): Int = {
    if (!memory.contains(name))
      memory(name) = 0
    memory(name)
  }

  // returns the (name) of a variable given its "address"
  def getNameOf(address: Int): String = {
    var currentAddress = 0
    for (tuple <- memory) {
      if (currentAddress == address)
        return tuple._1
      currentAddress += 1
    }
    throw new Error("Runtime Error: address not found!")
  }

  // runs the main execution line until end-of-program is reached
  def run(): Unit = {
    val execLine = parseTree.getBranches().iterator
    breakable {
      while (true) {
        val line = execLine.next()
        var lexeme = line.lexeme
        if (lexeme.token == Token.EO_PRG)
          break
        else if (lexeme.label.equals("line")) {
          val stmtOrComment = line.getBranches()(0)
          lexeme = stmtOrComment.lexeme
          if (lexeme.label.equals("statement"))
            run(stmtOrComment)
        }
        else
          throw new Error("Runtime Error: line expected!")
      }
    }
  }

  // runs a single (given) statement
  def run(stmt: Node): Unit = {
    val branch = stmt.getBranches()(0)
    val lexeme = branch.lexeme
    if (DEBUG) {
      displayStackAndMemory
      println(s"Stmt: ${lexeme}\n")
    }
    if (lexeme.token == Token.STRING)
      print(lexeme.label)
    else if (lexeme.token == Token.IDENTIFIER) {
      stack.push(addressOf(lexeme.label))
    } else if (lexeme.token == Token.LITERAL)
      stack.push(lexeme.label.toInt)
    else if (lexeme.token == Token.INPUT) {
      val anInt = StdIn.readInt()
      stack.push(anInt)
    }
    // TODO: implement the following condition according to expected semantics of the statement
    else if (lexeme.token == Token.OUTPUT) {

    }
    // TODO: implement the following condition according to expected semantics of the statement
    else if (lexeme.token == Token.ASSIGNMENT) {

    }
    else if (lexeme.token == Token.ADDITION) {
      val b = stack.pop
      val a = stack.pop
      stack.push(a + b)
    }
    else if (lexeme.token == Token.SUBTRACTION) {
      val b = stack.pop
      val a = stack.pop
      stack.push(a - b)
    }
    else if (lexeme.token == Token.MULTIPLICATION) {
      val b = stack.pop
      val a = stack.pop
      stack.push(a * b)
    }
    else if (lexeme.token == Token.DIVISION) {
      val b = stack.pop
      val a = stack.pop
      stack.push(a / b)
    }
    else if (lexeme.token == Token.MODULUS) {
      val b = stack.pop
      val a = stack.pop
      stack.push(a % b)
    }
    // TODO: implement the following condition according to expected semantics of the statement
    else if (lexeme.token == Token.LESS) {

    }
    // TODO: implement the following condition according to expected semantics of the statement
    else if (lexeme.token == Token.LESS_EQUAL) {

    }
    // TODO: implement the following condition according to expected semantics of the statement
    else if (lexeme.token == Token.GREATER) {

    }
    // TODO: implement the following condition according to expected semantics of the statement
    else if (lexeme.token == Token.GREATER_EQUAL) {

    }
    // TODO: implement the following condition according to expected semantics of the statement
    else if (lexeme.token == Token.EQUAL) {

    }
    // TODO: implement the following condition according to expected semantics of the statement
    else if (lexeme.token == Token.DIFFERENT) {
      
    }
    else if (lexeme.token == Token.DOT) {
      val address = stack.pop
      val name = getNameOf(address)
      stack.push(valueOf(name))
    }
    else if (lexeme.label.equals("if")) {
      val execLine = branch.getBranches().iterator
      execLine.next // consumes open bracket
      var done = false
      // check the if condition first
      if (stack.pop() != 0) {
        while (!done) {
          val line = execLine.next()
          var lexeme = line.lexeme
          if (lexeme.token == Token.CLOSE_BRACKET)
            done = true
          else if (lexeme.label.equals("line")) {
            val stmtOrComment = line.getBranches()(0)
            lexeme = stmtOrComment.lexeme
            if (lexeme.label.equals("statement"))
              run(stmtOrComment)
          }
          else
            throw new Error("Runtime Error: line expected!")
        } // end while
      } // end if
    } // end else if
    else if (lexeme.label.equals("while")) {
      var loopDone = false
      while (!loopDone) {
        val execLine = branch.getBranches().iterator
        execLine.next // consumes open parenthesis
        var iterationDone = false
        while (!iterationDone) {
          val line = execLine.next()
          var lexeme = line.lexeme
          if (lexeme.token == Token.CLOSE_PAR)
            iterationDone = true
          else if (lexeme.label.equals("line")) {
            val stmtOrComment = line.getBranches()(0)
            lexeme = stmtOrComment.lexeme
            if (lexeme.label.equals("statement")) {
              val stmt = stmtOrComment.getBranches()(0)
              lexeme = stmt.lexeme
              if (lexeme.token == Token.BREAK && stack.pop() != 0) {
                iterationDone = true
                loopDone = true
              }
              else
                run(stmtOrComment)
            } // end if
          } // end else if
          else
            throw new Error("Runtime Error: line expected!")
        } // end of iteration
      } // end of while
    } // end of if else
  }
}

object MouseInterpreter {

  def main(args: Array[String]): Unit = {

    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse
    val interpreter = new MouseInterpreter(parseTree)
    interpreter.run()
  }
}
