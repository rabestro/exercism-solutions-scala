import scala.annotation.tailrec

object AllYourBase:
  def rebase(inputBase: Int, digits: List[Int], outputBase: Int): Option[List[Int]] =
    if inputBase < 2 || outputBase < 2 || digits.exists(n => n < 0 || n >= inputBase) then
      None
    else
      val number = digits.foldLeft(0)(_ * inputBase + _)

      @tailrec
      def convert(number: Int, digits: List[Int]): List[Int] =
        if number == 0 then if digits.isEmpty then List(0) else digits
        else convert(number / outputBase, number % outputBase :: digits)

      Some(convert(number, Nil))
