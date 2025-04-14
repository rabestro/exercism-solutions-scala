sealed trait Bowling:
  def roll(pins: Int): Bowling

  def score(): Either[String, Int]

object Bowling:
  def apply(): Bowling = RunningGame(Nil, None)

case class IncorrectGame(reason: String) extends Bowling:
  override def roll(pins: Int): Bowling = this

  override def score(): Either[String, Int] = Left(reason)

case class RunningGame(completed: List[Frame], current: Option[Frame]) extends Bowling:
  private def isComplete: Boolean = completed.length == 10
    && (completed.head.isInstanceOf[OpenFrame]
    || current.isDefined && current.get.isInstanceOf[FillBallsFrame] && (
    completed.head.isInstanceOf[SpareFrame]
      && current.get.asInstanceOf[FillBallsFrame].rolls.length == 1
      || completed.head.isInstanceOf[StrikeFrame]
      && current.get.asInstanceOf[FillBallsFrame].rolls.length == 2
    )
    )

  override def roll(pins: Int): Bowling =
    current match
      case _ if isComplete =>
        IncorrectGame("Should not be able to roll after game is over")

      case _ if pins < 0 || pins > 10 =>
        IncorrectGame("Pins must have a value from 0 to 10")

      case Some(IncompleteFrame(first)) if first + pins > 10 =>
        IncorrectGame("Pin count exceeds pins on the lane")

      case Some(FillBallsFrame(List(first))) if first + pins > 10 =>
        IncorrectGame("Pin count exceeds pins on the lane")

      case Some(IncompleteFrame(first)) if first + pins == 10 =>
        RunningGame(SpareFrame(first, pins) :: completed, None)

      case Some(IncompleteFrame(first)) =>
        RunningGame(OpenFrame(first, pins) :: completed, None)

      case Some(FillBallsFrame(balls)) =>
        RunningGame(completed, Some(FillBallsFrame(pins :: balls)))

      case None if completed.length == 10 =>
        RunningGame(completed, Some(FillBallsFrame(List(pins))))

      case None if completed.length == 10 =>
        RunningGame(completed, Some(IncompleteFrame(pins)))

      case None if pins == 10 =>
        RunningGame(StrikeFrame() :: completed, None)

      case None =>
        RunningGame(completed, Some(IncompleteFrame(pins)))

      case Some(_) =>
        IncorrectGame("Game should not be in this state")

  override def score(): Either[String, Int] =
    if !isComplete then
      Left("Score cannot be taken until the end of the game")
    else
      Right(0)

