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
    def pascal(c: Int, r: Int): Int = {
      def loop(col: Int, row: Int): Int =
        if ( col == row || col == 0 ) 1
        else loop(col - 1, row - 1) + loop(col, row - 1)

      loop(c, r)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(str: List[Char], count: Int): Boolean = {
        if ( str.isEmpty ) {
          if (count == 0) true
          else false
        } else {
          val h = str.head
          if ( h == ')' )
            if ( count == 0) false
            else  loop(str.tail, count - 1)
          else
            if ( h == '(') loop(str.tail, count + 1)
            else  loop(str.tail, count)
        }
      }

      loop(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def loop(amount: Int, denoms: List[Int]): Int = {
        if ( amount == 0 )  1
        else if ( amount < 0 || denoms.isEmpty ) 0
        else  loop(amount, denoms.tail) + loop(amount - denoms.head, denoms)
      }

      loop(money, coins)
    }
  }
