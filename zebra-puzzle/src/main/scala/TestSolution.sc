import Category.*
import Relation.{LeftOf, NextTo, RightOf, Same}

type Subject = String
type House = Int
type Fact = (Subject, House)
type Statement = (Subject, Relation, Subject)

val facts: Map[Subject, House] = Map("Norwegian" -> 1, "milk" -> 3)

// facts and statements => new fact based on a statement

val st = ("blue", NextTo, "Norwegian")
Domain.occupiedHouses(facts)

val candidates = Domain.allStatements.filter {
  case (s1, _, s2) => !facts.keySet.contains(s1) && facts.keySet.contains(s2)
}

candidates.flatMap(Domain.findFact(_, facts)).headOption

