package recfun

import common._
import scala.annotation.tailrec
import sun.font.TrueTypeFont
import com.sun.org.apache.xpath.internal.functions.FuncFalse

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  /**
   * Compute Pascal triangle number corresponding to the specified column (c) and row (r).
   * WARNING: NOT tail-recursive.
   * @param c Column in the Pascal triangle (0-based).
   * @param r Row in the Pascal triangle (0-based).
   * @return Pascal triangle number.
   */
  def pascal(c: Int, r: Int): Int = {
    if ((c == 0) || (c == r)) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balance(count: Int, chars: List[Char]): Boolean = {
      if (count < 0) false
      else chars match {
        case Nil => (count == 0)
        case x :: xs => x match {
          case ')' => balance(count - 1, xs)
          case '(' => balance(count + 1, xs)
          case _ => balance(count, xs)
        }
      }
    }
    balance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(money: Int, coins: List[Int]): Int = {
      if (money < 0) 0
      else if (money == 0) 1
      else coins match {            // There is money left...
        case Nil => 0               // ... but no more coin: can't return exact change.
        case x :: xs => {           // ... and at least 1 coin left:
          count(money - x, coins) + // 1) we try to give the change using the current coin and try again
          count(money, xs)          // 2) we try to give the change using the remaining coins only
        }
      }
    }
    if (money > 0) count(money, coins) else 0
  }
}
