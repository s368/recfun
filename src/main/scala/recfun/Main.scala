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
      if(c == 0 || c == r) 1 
      else 
        pascal(c-1,r-1) + pascal(c,r-1)      
    }
  
  /**
   * Exercise 2
   */
    
    println("balance = " + balance("(just an example)".toList))
    //println("norm    = " + norm(0,"(just an example))".toList))
    
    def balance(chars: List[Char]): Boolean = {
      norm(0,chars) == 0
    }
    
    def norm(N: Int, chars: List[Char]): Int = chars match{
      case '(' :: rest => norm(+1 + N, rest)
      case ')' :: rest => if(N == 0) -1 else norm(-1+N, rest)
      case  x  :: rest => norm(N + 0, rest)
      case Nil         => N
    }
  
  /**
   * Exercise 3
   */
    
    //println("countChange = " + countChange(5,List(1,3)))
        
  def countChange(money: Int, coins: List[Int]): Int = {
  if(money == 0)
    1
  else if(money > 0 && !coins.isEmpty)
    countChange(money - coins.head, coins) + countChange(money, coins.tail)
  else
    0
}
}


