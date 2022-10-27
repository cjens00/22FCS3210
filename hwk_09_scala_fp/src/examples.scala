/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Student: 
 * Description: Homework 09 - Functional Programming Examples
 */

object Examples {

  // a) write function isPrime that takes an integer and returns true/false whether the input is a prime number or not.
  // A prime number (or a prime) is a natural number greater than 1 that is not a product of two smaller natural numbers.
  def isPrime(x: Int): Boolean = {
    if (x <= 1) throw new Exception("x has to be >= 1!")
    !(2 until x).exists(x % _ == 0)
  }

  // b) write function gcd that takes two integers and returns the GCD (Greatest Common Divisor) of the two inputs. Rewrite gcd as gcdC using currying notation.
  def gcd(a: Int, b: Int): Int = {
    if (a % b == 0) b else gcd(b, a % b)
  }

  def gcdc(a: Int): Int => Int =
    (b: Int) =>
      if (a % b == 0) b else gcdc(b)(a % b)

  // c) write function coprime that takes two integers and returns true/false whether the numbers are coprimes (their GCD equals to 1).
  def coprime(a: Int) =
    (b: Int) => gcdc(a)(b) == 1

  // d) write function totientPhi that takes an integer m and returns the positive integers r (1 <= r < m) that are coprime to m; challenge: implement totientPhi using until and filter functions.
  def totientPhi(m: Int) =
    (1 until m).filter((r: Int) => coprime(m)(r))

  // TODO #1: e) write function primeFactors that takes an integer and returns a flat list with the prime factors of the given number in ascending order.
  def primeFactors(x: Int) = { }

  // TODO #2: f) write function primeFactorsMult similar to primeFactors but with the prime factors and their multiplicity.
  def primeRange(x: Int) = { }

  // TODO #3: g) write function primesRange that takes a range of integers and returns a list of all prime numbers within that range.
  def primesRange(a: Int, b: Int) = { }

  // OPTIONAL TODO #1: h) Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers. Write function goldbach that takes an integer and returns the two prime numbers that sum up to it.
  def goldbach(x: Int) = { }

  // OPTIONAL TODO #2: i) write the function goldbachList that takes a range of integers and returns a list of all even numbers and their Goldbach composition.
  def goldbachList(a: Int, b: Int) = { }

  def main(args: Array[String]): Unit = {

  }
}