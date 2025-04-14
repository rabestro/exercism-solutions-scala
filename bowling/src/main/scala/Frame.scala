sealed trait Frame:
  def rolls: List[Int]

case class IncompleteFrame(first: Int) extends Frame:
  def rolls: List[Int] = List(first)

case class OpenFrame(first: Int, second: Int) extends Frame:
  def rolls: List[Int] = List(first, second)

case class SpareFrame(first: Int, second: Int) extends Frame:
  def rolls: List[Int] = List(first, second)

case class StrikeFrame() extends Frame:
  def rolls: List[Int] = List(10)

case class FillBallsFrame(rolls: List[Int]) extends Frame
