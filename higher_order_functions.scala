object hw3 {
  def zip(a: List[Int], b: List[Int], op: (Int, Int) => Int): List[Int] =
    if (a.isEmpty) b else
      if (b.isEmpty) a else
        op(a.head, b.head) :: zip(a.tail, b.tail, op)

  def flip(f: (Int, Int) => Int)(x: Int, y: Int) = f(y, x)

  def compose(f: Int => Int, g: Int => Int)(x: Int) = f(g(x))

  def combineNeighbors(lst: List[Int], op: (Int, Int) => Int): List[Int] =
    if (lst.isEmpty) lst else
      if (lst.tail.isEmpty) List(lst.head) else
        op(lst.head, lst.tail.head) :: combineNeighbors(lst.tail.tail, op)

  def iterateN(start: Int, f: Int => Int, n: Int): List[Int] = if (n == 0) Nil else
    start :: iterateN(f(start), f, n - 1)
    
  def iterateWhile(start: Int, f: Int => Int, p: Int => Boolean): List[Int] = if (!p(start)) Nil else
    start :: iterateWhile(f(start), f, p)
    
  def iterateUntil(start: Double, f: Double => Double, p: (Double, Double) => Boolean): List[Double] = {
    def helper(first: Double, second: Double, f: Double => Double, p: (Double, Double) => Boolean): List[Double] =
      first :: (if (p(first, second)) Nil else helper(second, f(second), f, p))
    helper(start, f(start), f, p)
  }
  
  def reduceWithDefault(defval: Int, lst: List[Int], op: (Int, Int) => Int): Int =
    if (lst.isEmpty) defval else
    reduceWithDefault(op(defval, lst.head), lst.tail, op)
    
  def otherReduceWithDefault(defval: Int, lst: List[Int], op: (Int, Int) => Int): Int = {
    def helper(lst: List[Int], op: (Int, Int) => Int, partialResult: Int): Int =
    	if (lst.isEmpty) partialResult else
    	helper(lst.tail, op, op(lst.head, partialResult))
    helper(lst, op, defval)
  }
  
  def both(f: Int => Boolean, g: Int => Boolean)(x: Int) = f(x) && g(x)
  
  def any(fs: List[Int => Boolean]): Int => Boolean = x => if (fs.isEmpty) false else fs.head(x) || any(fs.tail)(x)
}