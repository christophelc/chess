package model

import model.Square._

trait Square {
  def toInt: Int
  def whichCol: Col
  def whichRow: Row
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
  type Row = Byte
  type Col = Byte

  final val row2 = 1.toByte
  final val row7 = 6.toByte

}