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
  
  val allStatements: Set[(Subject, Relation, Subject)] = statements ++ reversedStatements
  
  val facts: Map[Subject, House] = Map("Norwegian" -> 1, "milk" -> 3)

  private def isValidHouse(house: House): Boolean = house >= 1 && house <= 5

  /**
   * Aggregates the houses occupied by subjects into categories, grouping the houses by their respective categories.
   *
   * @param facts A map where keys are subjects and values are the houses they occupy.
   * @return A map where keys are categories and values are sets of houses occupied by subjects in those categories.
   */
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

  /**
   * Determines a possible fact based on the given statement and the current known facts.
   *
   * @param statement The statement to evaluate, consisting of an unknown subject, a relation, and a known subject.
   * @param facts     A map of known associations between subjects and houses.
   * @return An Option containing the statement paired with a newly derived fact if determinable, or None otherwise.
   */
  def findFact(statement: Statement, facts: Map[Subject, House]): Option[(Statement, Fact)] =
    val (unknownSubject, relation, knownSubject) = statement
    val category = subjects(unknownSubject)
    val house = facts(knownSubject)
    val suggestions = relation match {
      case Same => Set(house)
      case RightOf => Set(house + 1)
      case LeftOf => Set(house - 1)
      case NextTo => Set(house + 1, house - 1)
    }
    val resultSet = suggestions.filter(isValidHouse).diff(occupiedHouses(facts).getOrElse(category, Set.empty))
    if resultSet.size == 1 then Some((statement, (unknownSubject, resultSet.head))) else None

end Domain