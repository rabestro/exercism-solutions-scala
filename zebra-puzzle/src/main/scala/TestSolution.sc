import Category.*
import Domain.{allStatements, facts}
import Relation.{LeftOf, NextTo, RightOf, Same}

// facts and statements => new fact based on a statement

val st = ("blue", NextTo, "Norwegian")
Domain.occupiedHouses(facts)
Domain.solve

val candidates = Domain.allStatements.filter {
  case (s1, _, s2) => !facts.keySet.contains(s1) && facts.keySet.contains(s2)
}

allStatements.size

val possibleFact = candidates
  .flatMap(Domain.findFact(_, facts))
  .headOption

val factsTwo = facts + possibleFact.get._2
val statsTwo = allStatements - possibleFact.get._1

statsTwo.size

val candidates2 = statsTwo.filter {
  case (s1, _, s2) => !factsTwo.keySet.contains(s1) && factsTwo.keySet.contains(s2)
}

