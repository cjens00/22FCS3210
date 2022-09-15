/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Activity 05 - Node (each tree node has a label, a key-value map, and branches)
 */

import scala.collection.mutable.ArrayBuffer

class Node(var lexeme: Lexeme) {

  private val branches = new ArrayBuffer[Node]()

  def add(branch: Node): Unit = {
    branches += branch
  }

  def getBranches() = branches

  private def print(current: Node, tabs: String): String = {
    var out = ""
    if (current == null)
      out
    else {
      out += tabs + current.lexeme + "\n"
      for (branch <- current.branches)
        out += print(branch, tabs + "\t")
      out
    }
  }

  override def toString = print(this, "")
}

// example code
object Node {
  def main(args: Array[String]): Unit = {
    val tree = new Node(new Lexeme("A"))
    val ab1 = new Node(new Lexeme("ab1"))
    val ab2 = new Node(new Lexeme("ab2"))
    val ab3 = new Node(new Lexeme("ab3"))
    val abc1 = new Node(new Lexeme("abc1"))
    val abc2 = new Node(new Lexeme("abc2"))
    tree.add(ab1)
    tree.add(ab2)
    tree.add(ab3)
    ab1.add(abc1)
    ab1.add(abc2)
    print(tree)
  }
}