import scala.annotation.{tailrec, targetName}

case class Clock (minutes: Int) {
  @targetName("add")
  def +(other: Clock): Clock = ???

  @targetName("minus")
  def -(other: Clock): Clock = ???
}

object Clock {
  private val MinutesInHour = 60
  private val MinutesInDay = MinutesInHour * 24

  def apply(hours: Int, minutes: Int): Clock =
    val totalMinutes = hours * MinutesInHour + minutes
    val normalizedMinutes = (totalMinutes % MinutesInDay + MinutesInDay) % MinutesInDay
    new Clock(normalizedMinutes)
}