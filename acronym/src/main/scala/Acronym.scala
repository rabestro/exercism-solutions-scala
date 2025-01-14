object Acronym {
  private val WordPattern = raw"\b_?(?<firstLetter>\w)[^-\s]*[- ]*".r

  def abbreviate(phrase: String): String =
    WordPattern.replaceAllIn(phrase, "${firstLetter}").toUpperCase
}
