package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c <= 0 || c >= r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], openCount: Int): Boolean = {
      if (chars.isEmpty) {
        openCount == 0
      } else {
        if (chars.head == '(') {
          balance(chars.tail, openCount + 1)
        } else if (chars.head == ')') {
          if (openCount <= 0) {
            false
          } else {
            balance(chars.tail, openCount - 1)
          }
        } else {
          balance(chars.tail, openCount)
        }
      }
    }
    balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) {
      1
    } else if(money < 0 || coins.isEmpty) {
      0
    } else {
      val coin = coins.head
      // Use coin or don't
      countChange(money - coin, coins) + countChange(money, coins.tail)
    }
  }
}
