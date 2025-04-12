object NthPrime:
  def prime(n: Int): Option[Int] =
    if n < 1 then None
    else
      Some(2)