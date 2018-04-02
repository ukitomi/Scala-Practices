
object recursion_practices {
  
  // Write a Scala function that produces the longest substring that is common to both a and b. 
  def lcs(a: String, b: String) : String = {
    def lcshelper(a: String, b: String, n: Int) : String = {
      if (n == 0) ""
      else if (b.contains(a.substring(0, n))) a.substring(0, n)
      else lcshelper(a, b, n - 1)
    }

    if (a.length == 0) a
    else if (a.length() > b.length()) lcs(b, a);
    else if (b contains a) a;
    else {
      val s1 = lcshelper(a, b, a.length())
      // If you instead use the following, you get O(2^n) running time
      // val s1 = lcs(a.substring(0, a.length() - 1), b) 
      val s2 = lcs(a.substring(1), b)
      if (s1.length() > s2.length()) s1 else s2
    }
  }

  // Write a Scala function that yields all “subset strings” of a given string, obtained by a subset of the letters in the same order. 
  def subs(s: String) : String = 
    if (s == "") "" else {
      val simplerResult = subs(s.substring(1))
      simplerResult + "|" + s.charAt(0) + simplerResult.replace("|", "|" + s.charAt(0))
    }  
  
  // Write a Scala function that returns a list of positions where the binary representation of n has a bit of 1. 
  def append(a: List[Int], b: List[Int]): List[Int] = if (a.isEmpty) b else
    a.head :: append(a.tail, b)
    // http://horstmann.com/sjsu/spring2018/cs152/lecture3/#(7)

  def onebits(n: Int) : List[Int] = if (n == 0) List() else 
    if (n < 0) {
      append(onebits(n & 0x7FFFFFFF), List(31))
    } else {
       val simplerResult = onebits(n / 2).map(x => x + 1)
       if (n % 2 == 0) simplerResult else 0 :: simplerResult      
    }
}
