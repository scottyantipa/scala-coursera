package recfun

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
    def pascal(c: Int, r: Int): Int = c match {
      case 0 | `r` => 1
      case _ => pascal(c - 1, r - 1) + pascal(c, r -1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def isBalanced(chars: List[Char], numOpen: Int): Boolean = {
        if (chars.isEmpty)
          true
        else
          chars.head match {
            case '(' => isBalanced(chars.tail, numOpen + 1)
            case ')' => numOpen > 0 && isBalanced(chars.tail, numOpen - 1)
            case _ => isBalanced(chars.tail, numOpen)
          }
      }

      isBalanced(chars, 0)
    }

  /**
   * Exercise 3
   (0, List()) -> 0
   (1, List()) -> 0
   (1, List(1)) -> 1
   (1, List(1,2)) -> 1
   (2, List(1)) -> 2

   (10, List(1,2,3))
    1:
      1:
        1:
        2:
        3:
      2:
      3:
    2:
      1:
      2:
      3:
    3:
      1:
      2:
      3:
    Sum all trees together:

    def calc(amount, coin, List(x,y,z)): Int ->
      remainder = amount - coin
      if remainder == 0 {
        1
      else
        coins match {
          case c::tail => calc(remainder, c, coins)
          case Nil => 0
    }
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty)
        0
      else if (money == 0)
        1
      else
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }

  }
