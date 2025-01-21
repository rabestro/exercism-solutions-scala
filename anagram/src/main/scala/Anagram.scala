object Anagram {
  def findAnagrams(word: String, candidates: List[String]): List[String] =
    val wordLowered = word.toLowerCase
    val wordSorted = wordLowered.sorted

    candidates.filter { candidate =>
      val lowered = candidate.toLowerCase
      lowered != wordLowered && lowered.sorted == wordSorted
    }
}
