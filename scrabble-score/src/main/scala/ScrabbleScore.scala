object ScrabbleScore:
  private def score(letter: Char): Int =
    letter.toUpper match
      case 'Q' | 'Z' => 10
      case 'J' | 'X' => 8
      case 'K' => 5
      case 'F' | 'H' | 'V' | 'W' | 'Y' => 4
      case 'B' | 'C' | 'M' | 'P' => 3
      case 'D' | 'G' => 2
      case _ => 1

  def score(word: String): Int = word.map(score).sum
