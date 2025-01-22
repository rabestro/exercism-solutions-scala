object Anagram:
  def findAnagrams(word: String, candidates: List[String]): List[String] =
    val wordSorted = word.toLowerCase.sorted

    candidates
      .filterNot(_.equalsIgnoreCase(word))
      .filter(_.toLowerCase.sorted == wordSorted)
