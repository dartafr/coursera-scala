package recfun
import common._
import scala.annotation.tailrec

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
  def pascal(colonne: Int, ligne: Int): Int = {
    if (colonne == 0 || colonne == ligne) {
      1
    } else {
      pascal(colonne - 1, ligne - 1) + pascal(colonne, ligne - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def checkBalance(chars: List[Char], stack: List[Char] = List()): Boolean = {
      if (chars.isEmpty) {
        stack.isEmpty;
      } else if (chars.head == '(') {
        checkBalance(chars.tail, chars.head :: stack)
      } else if (chars.head == ')') {
        !stack.isEmpty && stack.head != chars.head && checkBalance(chars.tail, stack.dropRight(1))
      } else {
        checkBalance(chars.tail, stack)
      }
    }
    checkBalance(chars);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (money < 0) {
      0
    } else if (coins.isEmpty) {
      0
    } else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
}
