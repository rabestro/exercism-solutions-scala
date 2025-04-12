object SpiralMatrix:
  def spiralMatrix(size: Int, start: Int = 1): List[List[Int]] = size match
    case 0 => Nil
    case 1 => List(List(start))
    case _ =>
      val step = size - 1
      val inner = spiralMatrix(size - 2, start + step * 4)
      val middle = 1 until step map { i =>
        val left = start + step * 4 - i
        val right = start + step + i
        left :: inner(i - 1) ::: right :: Nil
      }
      val top = start to start + step
      val bottom = start + step * 3 to start + step * 2 by -1
      top.toList :: middle.toList ::: bottom.toList :: Nil
