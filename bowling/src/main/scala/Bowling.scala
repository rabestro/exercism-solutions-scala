sealed trait Bowling:
  val MaxFrames = 10
  val MinPins = 0
  val MaxPins = 10

  def roll(pins: Int): Bowling

  def score(): Either[String, Int]

object Bowling:
  def apply(): Bowling = RunningGame(Nil, EmptyFrame)

case class RunningGame(completed: List[CompletedFrame], current: StateFrame) extends Bowling:

  override def roll(pins: Int): Bowling =
    if pins < MinPins || pins > MaxPins then
      IncorrectGame("Pins must have a value from 0 to 10")
    else current match
      case IncompleteFrame(first) if first + pins > MaxPins =>
        IncorrectGame("Pin count exceeds pins on the lane")

      case IncompleteFrame(first) if first + pins == MaxPins =>
        RunningGame(SpareFrame(first, pins) :: completed, EmptyFrame)

      case IncompleteFrame(first) =>
        val frames = OpenFrame(first, pins) :: completed
        frames.length match
          case MaxFrames => CompletedGame(frames.reverse)
          case _ => RunningGame(frames, EmptyFrame)

      case FillBallsFrame(balls) =>
        CompletedGame(completed.reverse, balls :+ pins)

      case EmptyFrame if completed.length == MaxFrames =>
        val fillBallsFrame = FillBallsFrame(List(pins))
        if completed.head.isInstanceOf[StrikeFrame] then
          RunningGame(completed, fillBallsFrame)
        else
          CompletedGame(completed.reverse, pins :: Nil)

      case EmptyFrame if pins == 10 =>
        RunningGame(StrikeFrame() :: completed, EmptyFrame)

      case EmptyFrame =>
        RunningGame(completed, IncompleteFrame(pins))


  override def score(): Either[String, Int] =
    Left("Score cannot be taken until the end of the game")

case class IncorrectGame(reason: String) extends Bowling:
  override def roll(pins: Int): Bowling = this

  override def score(): Either[String, Int] = Left(reason)

case class CompletedGame(frames: List[Frame], fillBalls: List[Int] = Nil) extends Bowling:
  override def roll(pins: Int): Bowling =
    IncorrectGame("Should not be able to roll after game is over")

  private def nextRolls(tail: List[CompletedFrame]): LazyList[Int] =
    LazyList.from(tail.flatMap(_.rolls) ::: fillBalls)

  private def scoreFrame(frame: CompletedFrame, nextFrames: List[CompletedFrame]): Int =
    frame match
      case _:StrikeFrame => 10 + nextRolls(nextFrames).take(2).sum
      case SpareFrame(first, second) => first + second + nextRolls(nextFrames).head
      case OpenFrame(first, second) => first + second



  override def score(): Either[String, Int] = Right(0)

