import scala.collection.mutable

object AllYourBase:
  def rebase(inputBase: Int, numbers: List[Int], outputBase: Int): Option[List[Int]] =
    val number = numbers.foldLeft(0)(_ * inputBase + _)
    def convert(number: Int, base: Int, result: mutable.ListBuffer[Int]): Unit =
      if number > 0 then
        convert(number / base, base, result)
        result.append(number % base)
    val result = mutable.ListBuffer[Int]()
    convert(number, outputBase, result)
    Some(result.toList)



