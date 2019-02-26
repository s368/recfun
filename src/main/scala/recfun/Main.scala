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
    
    println("balance = " + balance("(just an example".toList))
    println("norm    = " + norm(0,"(just an example))".toList))
    
    def balance(chars: List[Char]): Boolean = {
      if(norm(0,chars) != 0) false
      else 
        true
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
    
    print("counChange = " + countChange(5,List(1,3,5)))
    
    def countChange(money: Int, coins: List[Int]): Int = {
      var coef = List[Int]()
      var sum:Int = 0
      if(rest(money, coins, coef) == 0) {
        for(i <- 0 to coins.size-1){
          sum + coef(i)
        }  
      sum
      }
      else
          sum
    }
    
    def rest(money: Int, coins: List[Int], sum: Int): Int = {
      for(i <- 0 to coins.size-1){
       if(money  > coins(i)) rest(money - coins(i),coins, sum + 0)
       if(money == coins(i)) sum + 1
       if(money  < coins(i)) sum + 0
      }
      sum
    }
  }
