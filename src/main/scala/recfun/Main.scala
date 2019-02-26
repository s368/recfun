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
    
    print("counChange = " + countChange(5,List(1,3)))
    
    def countChange(money: Int, coins: List[Int]): Int = {
      var q1: Int = 0
      var q2: Int = 0
      var q3: Int = 0
      
      for(i <- 0 to coins.size-1){
       //println("i = " + i)
       if(money  > coins(i)) {
         q1 = countChange(money - coins(i),coins)
         println("q1 = " + coins(i))
       }
       if(money == coins(i)) {
         q2 = 1
         println("q2 = " + coins(i))
       }
       if(money  < coins(i)) {
         q3 = 0
         //println("q3 = " + coins(i))
       }
      }
      q1 + q2 + q3
    }
  }
