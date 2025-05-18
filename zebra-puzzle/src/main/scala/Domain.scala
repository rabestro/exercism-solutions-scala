import Category.*

type Subject = String
type House = Int
type Fact = (Subject, House)
type Statement = (Subject, Relation, Subject)

object Domain:
  val subjects: Map[Subject, Category] = Map(
    "Englishman" -> Person, "Spaniard" -> Person, "Ukrainian" -> Person, "Norwegian" -> Person, "Japanese" -> Person,
    "red" -> Color, "green" -> Color, "blue" -> Color, "yellow" -> Color, "ivory" -> Color,
    "dog" -> Pet, "snail" -> Pet, "horse" -> Pet, "zebra" -> Pet, "fox" -> Pet,
    "coffee" -> Beverage, "tea" -> Beverage, "milk" -> Beverage, "water" -> Beverage, "orange juice" -> Beverage,
    "dancing" -> Hobby, "painting" -> Hobby, "reading" -> Hobby, "football" -> Hobby, "chess" -> Hobby,
  )

  def occupiedHouses(facts: Map[Subject, House]): Map[Category, Set[House]] =
    facts.foldLeft(Map.empty[Category, Set[House]]) {
      case (acc, (subjectKey, houseValue)) =>
        subjects.get(subjectKey) match
          case Some(category) =>
            // updatedWith allows modifying an entry based on its current value or adding it if not present
            acc.updatedWith(category) {
              case Some(existingHouses) => Some(existingHouses + houseValue) // Add to existing set
              case None => Some(Set(houseValue)) // Create new set with the house
            }
          case None => acc
    }

end Domain