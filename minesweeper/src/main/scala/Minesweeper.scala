object Minesweeper:
  def annotate(field: List[String]): List[String] =
    val rows = field.length
    val cols = field.headOption.map(_.length).getOrElse(0)
    val result = field

    def countMines(row: Int, col: Int): Int =
      (row - 1 to row + 1)
        .filter(r => r >= 0 && r < rows)
        .flatMap(r =>
          (col - 1 to col + 1)
            .filter(c => c >= 0 && c < cols)
            .map(c => result(r)(c))
        ).count(_ == '*')

    def annotateCell(row: Int, col: Int): String =
      if field(row)(col) == '*' then "*"
      else countMines(row, col) match
        case 0 => " "
        case n => n.toString

    def annotateRow(row: Int): String =
      field(row).zipWithIndex.map { case (c, i) => annotateCell(row, i) }.mkString

    (0 until rows).map(annotateRow).toList  
  

