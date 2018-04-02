object higher_order_functions {
  
  // Write a function zip that accepts two List[Int] and an (Int, Int) => Int function, and that returns a list of results of evaluating the function on corresponding elements. 
  // If one list is longer than the other, append the unmatched tail. 
  def zip(a: List[Int], b: List[Int], op: (Int, Int) => Int): List[Int] =
    if (a.isEmpty) b else
      if (b.isEmpty) a else
        op(a.head, b.head) :: zip(a.tail, b.tail, op)

  // Write a function flip that, given an (Int, Int) => Int function, yields an (Int, Int) => Int function with the arguments flipped. 
  def flip(f: (Int, Int) => Int)(x: Int, y: Int) = f(y, x)

  // Write a function compose that yields the Int => Int function obtained by composing two Int => Int functions (i.e. applies the second function, then the first)
  def compose(f: Int => Int, g: Int => Int)(x: Int) = f(g(x))

  // Write a function combineNeighbors that accepts a List[Int] and an (Int, Int) => Int function. 
  // Return a List[Int] with the result of combining neighboring elements with the given function. If the list is odd, append the last element. 
  def combineNeighbors(lst: List[Int], op: (Int, Int) => Int): List[Int] =
    if (lst.isEmpty) lst else
      if (lst.tail.isEmpty) List(lst.head) else
        op(lst.head, lst.tail.head) :: combineNeighbors(lst.tail.tail, op)

  // Write a function iterateN that takes a starting Int value x, a function f of type Int => Int, and an integer n, and produces a list of length n containing x, f(x), f(f(x)), f(f(f(x))), and so on.
  def iterateN(start: Int, f: Int => Int, n: Int): List[Int] = if (n == 0) Nil else
    start :: iterateN(f(start), f, n - 1)
    
  // Write a function iterateWhile that takes a starting Int value x, a function f of type Int => Int, and a function p of type Int => Bool, and produces a list containing x, f(x), f(f(x)), f(f(f(x))), 
  // and so on, as long as p returns true for the function values. 
  def iterateWhile(start: Int, f: Int => Int, p: Int => Boolean): List[Int] = if (!p(start)) Nil else
    start :: iterateWhile(f(start), f, p)
  
  // Write a function iterateUntil that takes a starting Double value x, a function f of type Double => Double, and a function p of type (Double, Double) => Bool, 
  // and produces a list containing x, f(x), f(f(x)), f(f(f(x))), and so on, as long as p returns false for the last two function values. 
  def iterateUntil(start: Double, f: Double => Double, p: (Double, Double) => Boolean): List[Double] = {
    def helper(first: Double, second: Double, f: Double => Double, p: (Double, Double) => Boolean): List[Double] =
      first :: (if (p(first, second)) Nil else helper(second, f(second), f, p))
    helper(start, f(start), f, p)
  }
  
  // Write a function reduceWithDefault that combine the default with the first element, then the result with the second element, and so on. 
  def reduceWithDefault(defval: Int, lst: List[Int], op: (Int, Int) => Int): Int =
    if (lst.isEmpty) defval else
    reduceWithDefault(op(defval, lst.head), lst.tail, op)
    
  // Write a function otherReduceWithDefault that combine the last element with the default, then the penultimate list element with the result, and so on. 
  def otherReduceWithDefault(defval: Int, lst: List[Int], op: (Int, Int) => Int): Int = {
    def helper(lst: List[Int], op: (Int, Int) => Int, partialResult: Int): Int =
    	if (lst.isEmpty) partialResult else
    	helper(lst.tail, op, op(lst.head, partialResult))
    helper(lst, op, defval)
  }
  
  // Write a function both that receives two Int => Boolean functions and returns an Int => Boolean function that returns true when both input functions return true. 
  def both(f: Int => Boolean, g: Int => Boolean)(x: Int) = f(x) && g(x)
  
  // Write a function any that receives a List[Int => Boolean] and returns an Int => Boolean function that returns true when any functions in the list return true.
  def any(fs: List[Int => Boolean]): Int => Boolean = x => if (fs.isEmpty) false else fs.head(x) || any(fs.tail)(x)
}
