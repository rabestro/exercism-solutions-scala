import scala.collection.mutable

object Sieve:
  def primes(number: Int): List[Int] =
    val unmarkedNumbers = mutable.BitSet(2 to number *)
    val limit = math.sqrt(number).toInt

    for (x <- 2 to limit if unmarkedNumbers contains x) do
      for (multiple <- x * x to number by x) do
        unmarkedNumbers -= multiple

    unmarkedNumbers.toList      
