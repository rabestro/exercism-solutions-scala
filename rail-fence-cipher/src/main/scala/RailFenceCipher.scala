object RailFenceCipher:
  def encode(plainText: String, rails: Int): String =
    val maxRow = rails - 1
    val maxCol = plainText.length - 1
    val step = 2 * maxRow
    val top = 0 to maxCol by step
    val bottom = maxRow to maxCol by step
    val inners = for {
      row <- 1 until maxRow
      first <- row to maxCol by step
      second = first + 2 * (rails - row - 1)
      index <- if second <= maxCol then Seq(first, second) else Seq(first)
    } yield index
    (top ++ inners ++ bottom).map(plainText).mkString
  end encode

  def decode(encodedText: String, rails: Int): String = encodedText

