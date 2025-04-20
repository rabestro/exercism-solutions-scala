import scala.collection.SortedSet

object Change:
  def findFewestCoins(amount: Int, denominations: List[Int]): Option[List[Int]] =
    greedy(amount, SortedSet(denominations *))


  def greedy(amount: Int, denominations: SortedSet[Int]): Option[List[Int]] =
    def change(amount: Int, result: List[Int]): Option[List[Int]] =
      if amount <= 0 then Option(result)
      else denominations.maxBefore(amount + 1)
        .flatMap(coin => change(amount - coin, coin :: result))

    change(amount, Nil)
