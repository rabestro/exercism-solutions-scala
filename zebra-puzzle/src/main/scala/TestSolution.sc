import Category.*
import Relation.{LeftOf, NextTo, RightOf, Same}

type Subject = String
type House = Int
type Fact = (Subject, House)
type Statement = (Subject, Relation, Subject)

val subjects: Map[Subject, Category] = Map(
  "Englishman" -> Person, "Spaniard" -> Person, "Ukrainian" -> Person, "Norwegian" -> Person, "Japanese" -> Person,
  "red" -> Color, "green" -> Color, "blue" -> Color, "yellow" -> Color, "ivory" -> Color,
  "dog" -> Pet, "snail" -> Pet, "horse" -> Pet, "zebra" -> Pet, "fox" -> Pet,
  "coffee" -> Beverage, "tea" -> Beverage, "milk" -> Beverage, "water" -> Beverage, "orange juice" -> Beverage,
  "dancing" -> Hobby, "painting" -> Hobby, "reading" -> Hobby, "football" -> Hobby, "chess" -> Hobby,
)

val facts: Map[Subject, House] = Map("Norwegian" -> 1, "milk" -> 3)

// facts and statements => new fact based on a statement

def isValidHouse(house: House): Boolean = house >= 1 && house <= 5

def findFact(statement: Statement): Option[Fact] = {
  val (unknownSubject, relation, knownSubject) = statement
  val houseOption = facts.get(knownSubject)
  val category = subjects(unknownSubject)
  val occupiedHouses = facts.filter(pair => subjects(pair._1) == category).values
  val house = houseOption.getOrElse(-10)
  val suggestions = relation match {
    case Same => Set(house)
    case RightOf => Set(house + 1)
    case LeftOf => Set(house - 1)
    case NextTo => Set(house + 1, house - 1)
  }
  val set = suggestions.filter(isValidHouse).filterNot(house => occupiedHouses.exists(h => h == house))
  if set.size == 1 then Some((unknownSubject, set.head)) else None
}

val st = ("blue", NextTo, "Norwegian")
findFact(st)



Domain.occupiedHouses(facts)
