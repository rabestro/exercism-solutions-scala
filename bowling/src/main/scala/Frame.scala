sealed trait Frame

sealed trait CompletedFrame extends Frame:
  def rolls: List[Int]

case class OpenFrame(first: Int, second: Int) extends CompletedFrame:
  def rolls: List[Int] = List(first, second)

case class SpareFrame(first: Int, second: Int) extends CompletedFrame:
  def rolls: List[Int] = List(first, second)

case class StrikeFrame() extends CompletedFrame:
  def rolls: List[Int] = List(10)

sealed trait StateFrame extends Frame

case object EmptyFrame extends StateFrame

case class IncompleteFrame(first: Int) extends StateFrame

case class FillBallsFrame(rolls: List[Int]) extends StateFrame
