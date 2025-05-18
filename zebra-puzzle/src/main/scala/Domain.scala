import Category.*
import Relation.{LeftOf, NextTo, RightOf, Same}

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

  private val statements: Set[Statement] = Set(
    ("Englishman", Same, "red"), ("Spaniard", Same, "dog"), ("green", Same, "coffee"),
    ("Ukrainian", Same, "tea"), ("green", RightOf, "ivory"), ("snail", Same, "dancing"),
    ("yellow", Same, "painting"), ("reading", NextTo, "fox"), ("painting", NextTo, "horse"),
    ("football", Same, "orange juice"), ("Japanese", Same, "chess"), ("Norwegian", NextTo, "blue")
  )

  private val reversedStatements = statements.map {
    case (s1, RightOf, s2) => (s2, LeftOf, s1)
    case (s1, LeftOf, s2) => (s2, RightOf, s1)
    case (s1, relation, s2) => (s2, relation, s1)
  }
  
  val allStatements = statements ++ reversedStatements
  
  val facts: Map[Subject, House] = Map("Norwegian" -> 1, "milk" -> 3)

  def isValidHouse(house: House): Boolean = house >= 1 && house <= 5

  def occupiedHouses(facts: Map[Subject, House]): Map[Category, Set[House]] =
    facts.foldLeft(Map.empty[Category, Set[House]]) {
      case (acc, (subjectKey, houseValue)) =>
        subjects.get(subjectKey) match
          case Some(category) =>
            acc.updatedWith(category) {
              case Some(existingHouses) => Some(existingHouses + houseValue)
              case None => Some(Set(houseValue))
            }
          case None => acc
    }

end Domain