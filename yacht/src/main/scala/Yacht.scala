object Yacht:

  def score(dices: List[Int], category: String): Int =

    lazy val isYacht = dices.distinct.size == 1

    lazy val isFullHouse =
      dices.groupBy(identity).values.map(_.size).toSet == Set(2, 3)

    lazy val isFourOfAKind =
      dices.distinct.size < 3 && !isFullHouse

    category match
      case "ones" => dices.count(_ == 1)
      case "twos" => dices.count(_ == 2) * 2
      case "threes" => dices.count(_ == 3) * 3
      case "fours" => dices.count(_ == 4) * 4
      case "fives" => dices.count(_ == 5) * 5
      case "sixes" => dices.count(_ == 6) * 6
      case "full house" if isFullHouse => dices.sum
      case "four of a kind" if isFourOfAKind => dices.sorted.drop(1).head * 4
      case "little straight" if dices.toSet == (1 to 5).toSet => 30
      case "big straight" if dices.toSet == (2 to 6).toSet => 30
      case "yacht" if isYacht => 50
      case "choice" => dices.sum
      case _ => 0
