object Acronym:
  private val FirstLetter = raw"(?<![\p{Alpha}'])\p{Alpha}".r

  def abbreviate(phrase: String): String =
    FirstLetter.findAllIn(phrase).mkString.toUpperCase
