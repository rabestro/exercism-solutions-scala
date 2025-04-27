case class Garden(var plants: List[List[Plant]]):
  def plants(child: String): List[Plant] =
    val index = child.head - 'A'
    plants(index)

object Garden:
  def charToPlant(c: Char): Option[Plant] = c match {
    case 'G' => Some(Plant.Grass)
    case 'C' => Some(Plant.Clover)
    case 'R' => Some(Plant.Radishes)
    case 'V' => Some(Plant.Violets)
    case _ => None
  }

  def defaultGarden(diagram: String): Garden =
    val lines = diagram.split('\n').map(_.grouped(2)).toList
    val pairedGroups = lines.head.zip(lines.tail.head)
    val garden = pairedGroups.map {
      case (pair1, pair2) => (pair1 + pair2).toList.flatMap(charToPlant)
    }
    Garden(garden.toList)

enum Plant:
  case Grass, Clover, Radishes, Violets