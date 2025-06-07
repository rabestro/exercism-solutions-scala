object RailFenceCipher:
  def encode(plainText: String, rails: Int): String =
    (0 until rails).flatMap { row =>
      val delta = if row % (rails - 1) == 0 then 2 * (rails - 1) else rails - 1
      (row until plainText.length by delta).map(col => plainText(col))
    }.mkString

  def decode(encodedText: String, rails: Int): String = encodedText

