object RailFenceCipher:
  def encode(plainText: String, rails: Int): String =
    val maxRow = rails - 1
    val encodedChars = for {
      row <- 0 to maxRow
      col <- row until plainText.length by 2 * maxRow
      innerRow = row > 0 && row < maxRow
      index <- if innerRow then Seq(col, col + 2 * (rails - row) - 2) else Seq(col)
      if index < plainText.length
    } yield plainText(index)
    encodedChars.mkString
  end encode

  def decode(encodedText: String, rails: Int): String = encodedText

