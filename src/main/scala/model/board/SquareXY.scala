package model.board

import model.Square._
import model._

case class SquareXY(val row: Row, val col: Col) extends Square {
  override def toInt: Int = this.row.value * 8 + this.col.value
  override def whichRow: Row = row
  override def whichCol: Col = col
  override def isInsideChessboard: Boolean = row.isValid && col.isValid
  override def isRowLimit: Boolean = row.isLimit

  override def left: Square = SquareXY(row, col.minus(1))
  override def right: Square = SquareXY(row, col.add(1))
  override def up: Square = SquareXY(row.add(1), col)
  override def down: Square = SquareXY(row.minus(1), col)
  override def shift(direction: Direction): Square = this.copy(
    row = row.add(direction.vertical),
    col = col.add(direction.horizontal))

  override def color: Color = if (row.value + col.value % 2 == 1) White else Black
  private def directionTo(square: Square): Option[Direction] = {
    val dcol = square.whichCol.minus(col)
    val drow = square.whichRow.minus(row)
    (dcol, drow) match {
      case (0, _) => Some(Direction(vertical = math.signum(drow).toByte))
      case (_, 0) => Some(Direction(horizontal = math.signum(dcol).toByte))
      case (_, _) => if (math.abs(dcol) == math.abs(drow)) {
        Some(Direction(horizontal = math.signum(dcol).toByte, vertical = math.signum(drow).toByte))
      } else
        None
    }
  }
  override def squaresStrictlyBetween(square: Square): Seq[Square] = {
    directionTo(square).toSeq.flatMap(direction => {
      val n = math.max(math.abs(square.whichRow.minus(row)), math.abs(square.whichCol.minus(col))) - 1
      for (i <- 1 to n) yield SquareXY(row.add(i * direction.vertical), col.add(i * direction.horizontal))
    })
  }
  override def show: String = "☐"

  override def toString: String = "ABCDEFGH".charAt(col.value).toString + "12345678".charAt(row.value).toString
}

object RichSquare {
  implicit class SquareXYFromString(val square: String) {
    def toSquare: SquareXY = {
      require(square.length == 2 && square(0).toLower >= 'a' && square(0).toLower <= 'h' && square(1) >= '1' && square(1) <= '8')
      SquareXY(row = Row(square(1) - '1'), col = Col(square(0).toLower - 'a'))
    }
  }
}