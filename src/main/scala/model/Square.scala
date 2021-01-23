package model

import model.Square._

object Row {
  def apply(value: Int): Row = new Row(value.toByte)
}

case class Row(value: Byte) extends AnyVal {
  def isLimit: Boolean = value == 0 || value == 7
  def isValid: Boolean = value >= 0 && value <= 7
  def add(row: Row): Row = Row((value + row.value).toByte)
  def add(n: Int): Row = Row((value + n).toByte)
  def minus(n: Int): Row = Row((value - n).toByte)
  def minus(row: Row): Byte = (value - row.value).toByte
}

object Col {
  def apply(value: Int): Col = new Col(value.toByte)
}

case class Col(value: Byte) extends AnyVal {
  def isValid: Boolean = value >= 0 && value <= 7
  def add(col: Col): Col = Col((value + col.value).toByte)
  def add(n: Int): Col = Col((value + n).toByte)
  def minus(n: Int): Col = Col((value - n).toByte)
  def minus(col: Col): Byte = (value - col.value).toByte
}

trait Square {
  def toInt: Int
  def whichCol: Col
  def whichRow: Row
  def isInsideChessboard: Boolean
  def isRowLimit: Boolean
  def left: Square
  def right: Square
  def up: Square
  def down: Square
  def shift(direction: Direction): Square
  def color: Color

  /**
   * Return a sequence of square strictly between to Squares, if it makes sense.
   * If the square are on the same line or diagonal, it makes sense.
   * In the contrary, this function return Nil.
   * @param square
   * @return
   */
  def squaresStrictlyBetween(square: Square): Seq[Square]
  def show: String
}

object Square {
  final val row1 = Row(0)
  final val row2 = Row(1)
  final val row3 = Row(2)
  final val row4 = Row(3)
  final val row5 = Row(4)
  final val row6 = Row(2)
  final val row7 = Row(6)
  final val row8 = Row(7)
  final val colA = Col(0)
  final val colB = Col(1)
  final val colC = Col(2)
  final val colD = Col(3)
  final val colE = Col(4)
  final val colF = Col(5)
  final val colG = Col(6)
  final val colH = Col(7)
}