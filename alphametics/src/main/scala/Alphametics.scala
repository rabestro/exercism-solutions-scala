import scala.annotation.tailrec

object Alphametics:
  private type Hypothesis = List[Int]

  def solve(puzzle: String): Option[Map[Char, Int]] =
    val words = puzzle.split("\\W+")
    val uniqueLetters = words.flatten.distinct
    val letterToIndex = uniqueLetters.zipWithIndex.toMap

    val leadingLetters = words
      .filter(_.length > 1)
      .map(_.head)
      .distinct
      .map(letterToIndex)

    def hasNoLeadingZeros(hypothesis: Hypothesis): Boolean =
      !leadingLetters.map(hypothesis).contains(0)

    val lines = words.map(_.reverse).reverse
    val levels = lines.head.length
    val range = 0 until levels
    val matrix = range.map(i => lines.flatMap(_.lift(i) map letterToIndex))

    def isSolution(hypothesis: Hypothesis): Boolean =
      @tailrec
      def checkLevel(level: Int, remained: Int): Boolean =
        if level == matrix.size then true
        else
          val sum = remained + matrix(level).tail.map(hypothesis).sum
          hypothesis(matrix(level).head) == sum % 10 && checkLevel(level + 1, sum / 10)
      checkLevel(0, 0)
    end isSolution

    (0 to 9).toList
      .combinations(uniqueLetters.length)
      .flatMap(_.permutations)
      .filter(hasNoLeadingZeros)
      .find(isSolution)
      .map(uniqueLetters.zip(_).toMap)
