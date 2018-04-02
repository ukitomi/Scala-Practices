import scala.util.matching.Regex

object map_and_tuples {

  // First, accept two lists of regex, the tokens that should be reported and those that should be ignored. (The latter would contain white space and comments). 
  // Next, return a pair consisting of a list of tokens that you found and an integer that indicates the index of the first error—the position in the original string where you were unable to find a match. 
  // If the entire input string matched, return -1
  def firstMatch(input: String, patterns: List[Regex]): String =
    if (input.isEmpty || patterns.isEmpty) null else {
      val matched = patterns.head.findPrefixOf(input).getOrElse(null)
      if (matched != null) matched else firstMatch(input, patterns.tail)
    }

  def tokens(input: String, patterns: List[Regex], ignorePatterns: List[Regex]): (List[String], Int) = {
    if (input.isEmpty) (Nil, -1) else {
      val ignored = firstMatch(input, ignorePatterns)
      if (ignored == null) {
        val matched = firstMatch(input, patterns)
        if (matched == null) (Nil, 0) else {
          val result = tokens(input.substring(matched.length), patterns, ignorePatterns)
          (matched :: result._1, if (result._2 == -1) -1 else result._2 + matched.length)
        }
      } else {
        val result =
          tokens(input.substring(ignored.length), patterns, ignorePatterns)
        (result._1, if (result._2 == -1) -1 else result._2 + ignored.length)
      }
    }
  }

  // ------------------------------------------------------------------------------------------------------------------

  // Phone mnemonics problem
  val characters = (s: String) => s.toList.map("" + _)

  val letters = Map("2" -> "ABC", "3"-> "DEF", "4"-> "GHI", "5" -> "JKL", "6" -> "MNO", "7" -> "PRS", "8" -> "TUV", "9" -> "WXY").map(e => (e._1, characters(e._2)))

  val words = io.Source.fromURL("http://horstmann.com/sjsu/spring2018/cs152/words").getLines.filter(w => Character.isLowerCase(w(0)) && w.length > 1).map(_.toUpperCase).toSet + "SCALA"

  val cats = (s: List[String], t: List[String]) => t.flatMap(y => s.map(x => x + y))
  
  val wordsForDigits = (digits: String) => characters(digits).map(c => letters(c)).reduceLeft(cats(_, _)).filter(words.contains(_))

  val catsSpaces = (s: List[String], t: List[String]) => t.flatMap(y => s.map(x => x + " " + y))

  val wordsForDigitsSequence = (seq: List[String]) =>
    seq.map(e => wordsForDigits(e)).reduceLeft(catsSpaces)

  val grow = (c: String, a: List[List[String]]) => a.map(s => c :: s) ++ a.map(s => (c + s.head) :: s.tail)

  val substrings = (s: String) => characters(s.substring(0, s.length - 1)).foldRight(List(List(s.substring(s.length - 1))))(grow)

  val phoneMnemonics = (digits: String) => substrings(digits).flatMap(wordsForDigitsSequence)
}
