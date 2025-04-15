object PythagoreanTriplet:
  def isPythagorean(candidate: (Int, Int, Int)): Boolean =
    val (a, b, c) = candidate
    a < b && b < c && a * a + b * b == c * c

  def pythagoreanTriplets(min: Int, max: Int): Seq[(Int, Int, Int)] =
    for {
      a <- min to max - 2
      b <- a until max
      c <- b to max
      candidate  = (a, b, c)
      if isPythagorean(candidate)
    } yield candidate

  def pythagoreanTripletsSum(sum: Int): Seq[(Int, Int, Int)] = Nil

