object PythagoreanTriplet:
  def isPythagorean(candidate: (Int, Int, Int)): Boolean =
    val (a, b, c) = candidate
    a < b && b < c && a * a + b * b == c * c

  def pythagoreanTriplets(from: Int, to: Int): Seq[(Int, Int, Int)] = Nil

  def pythagoreanTripletsSum(sum: Int): Seq[(Int, Int, Int)] = Nil

