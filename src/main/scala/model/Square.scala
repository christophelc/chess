package model

trait Square {
  def toInt: Int
  def whichCol: Int
  def whichRow: Int
  def isInsideChessboard: Boolean
  def isVerticalBorder: Boolean
  def left: Square
  def right: Square
  def up: Square
  def down: Square
  def shift(direction: Direction): Square
  def color: Color
  def squaresStrictlyBetween(square: Square): Seq[Square]
  def show: String
}

object Square {
  final val row2 = 1
  final val row7 = 6
}

object RichSquare {
  implicit class SquareXYFromString(val square: String) {
    def toSquare: SquareXY = {
      require(square.length == 2 && square(0).toLower >= 'a' && square(0).toLower <= 'h' && square(1) >= '1' && square(1) <= '8')
      SquareXY(row = square(1) - '1', col = square(0).toLower - 'a')
    }
  }
}

case class SquareXY(val row: Int, val col: Int) extends Square {
  override def toInt: Int = this.row * 8 + this.col
  override def whichRow: Int = row
  override def whichCol: Int = col
  override def isInsideChessboard: Boolean = row >= 0 && row <= 7 && col >= 0 && col <= 7
  override def isVerticalBorder: Boolean = row == 0 || row == 7

  override def left: Square = SquareXY(row, col - 1)
  override def right: Square = SquareXY(row, col + 1)
  override def up: Square = SquareXY(row + 1, col)
  override def down: Square = SquareXY(row - 1, col)
  override def shift(direction: Direction): Square = this.copy(
    row = row + direction.vertical,
    col = col + direction.horizontal)

  override def color: Color = if (row + col % 2 == 1) White else Black
  private def directionTo(square: Square): Option[Direction] = {
    val dcol = square.whichCol - col
    val drow = square.whichRow - row
    (dcol, drow) match {
      case (0, _) => Some(Direction(vertical = math.signum(drow)))
      case (_, 0) => Some(Direction(horizontal = math.signum(dcol)))
      case (_, _) => if (math.abs(dcol) == math.abs(drow)) {
        Some(Direction(horizontal = math.signum(dcol), vertical = math.signum(drow)))
      } else
        None
    }
  }
  override def squaresStrictlyBetween(square: Square): Seq[Square] = {
    directionTo(square).toSeq.flatMap(direction => {
      val n = math.max(math.abs(square.whichRow - row), math.abs(square.whichCol - col)) - 1
      for (i <- 1 to n) yield SquareXY(row + i * direction.vertical, col + i * direction.horizontal)
    })
  }
  override def show: String = "☐"

  override def toString: String = "ABCDEFGH".charAt(col).toString + "12345678".charAt(row).toString
}