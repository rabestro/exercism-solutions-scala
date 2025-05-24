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

  val solve: Option[Map[Subject, House]] = solution(facts, allStatements)

  def solution(facts: Map[Subject, House], statements: Set[(Subject, Relation, Subject)]
              ): Option[Map[Subject, House]] =


    val relevantStatements = statements.filter {
      case (s1, _, s2) => !facts.keySet.contains(s1) && facts.keySet.contains(s2)
    }
    println(s"Known facts: ${facts.size}, Statements: ${statements.size}")
    val occupied = occupiedHouses(facts)

    def possibleHouses(statement: Statement): (Statement, Set[House]) =
      val (unknownSubject, relation, knownSubject) = statement
      val category = subjects(unknownSubject)
      val house = facts(knownSubject)
      val suggestions = relation match {
        case Same => Set(house)
        case RightOf => Set(house + 1)
        case LeftOf => Set(house - 1)
        case NextTo => Set(house + 1, house - 1)
      }
      val houses = suggestions.filter(isValidHouse) diff occupied.getOrElse(category, Set.empty)
      statement -> houses

    if relevantStatements.isEmpty then
      println("No relevant statements found")
      None
    else
      val candidateHouses = relevantStatements.map(possibleHouses)
      println("Possible Houses for relevant statements:")
      candidateHouses.foreach(println)
      if candidateHouses.exists(_._2.isEmpty) then
        println("No solution")
        None
      else {
        val preciseStatements = candidateHouses.filter(_._2.size == 1).map(_._1)
        val determinedAssignments = candidateHouses.filter(_._2.size == 1).map(e => e._1._1 -> e._2.head)
        if determinedAssignments.isEmpty then
          println("No new facts found")
          None
        else
          println(s"Found ${determinedAssignments.size} new facts")
          determinedAssignments.foreach(println)
          solution(facts ++ determinedAssignments, statements -- preciseStatements)

      }


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