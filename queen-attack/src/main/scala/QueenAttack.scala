import scala.math.abs

object QueenAttack:
  def canAttack(whiteQueen: Queen, blackQueen: Queen): Boolean =
    whiteQueen canAttack blackQueen

object Queen:
  private def isValidPosition(x: Int, y: Int) =
    x >= 0 && x <= 7 && y >= 0 && y <= 7

  def create(x: Int, y: Int): Option[Queen] =
    (Option when isValidPosition(x, y))(Queen(x, y))

case class Queen(row: Int, col: Int):
  infix def canAttack(that: Queen): Boolean =
    row == that.row || col == that.col ||
      abs(col - that.col) == abs(row - that.row)
