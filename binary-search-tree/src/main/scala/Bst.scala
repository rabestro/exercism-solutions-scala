

case class Bst(value: Int, left: Option[Bst] = None, right: Option[Bst] = None):
  def insert(x: Int): Bst =
    if x <= value then
      Bst(value, left.map(_.insert(x)) orElse Some(Bst(x)), right)
    else
      Bst(value, left, right.map(_.insert(x)) orElse Some(Bst(x)))
      
object Bst:
  def toList(bst: Bst): List[Int] =
    bst.left match
      case Some(node) => node.value :: toList(node)
      case None => List(bst.value)

  def fromList(list: List[Int]): Bst =
     Bst(0)
      
      