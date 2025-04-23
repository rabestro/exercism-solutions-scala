import scala.collection.mutable

object AllYourBase:
  def rebase(inputBase: Int, numbers: List[Int], outputBase: Int): Option[List[Int]] =
    if inputBase < 2 || outputBase < 2 || numbers.exists(n => n < 0 || n >= inputBase) then None
    else
      val number = numbers.foldLeft(0)(_ * inputBase + _)
      def convert(number: Int, result: mutable.ListBuffer[Int]): Unit =
        if number > 0 then
          convert(number / outputBase, result)
          result.append(number % outputBase)
      val result = mutable.ListBuffer[Int]()
      convert(number, result)
      if result.isEmpty then Some(List(0))
      else Some(result.toList)



