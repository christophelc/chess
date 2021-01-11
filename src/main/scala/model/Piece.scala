package model

import model.Piece._
import model.board.RichSquare.SquareXYFromString
import model.Square.{ row2, row7 }
import model.board.ChessboardImpl

object Piece {
  type PieceId = Byte
  final val idQueen: PieceId = 0
  final val idKing: PieceId = 1
  final val idBishop: PieceId = 2
  final val idKnight: PieceId = 3
  final val idRook: PieceId = 4
  final val idPawn: PieceId = 5
  def verticalDirection(color: Color): Direction = color match {
    case White => Direction.up
    case Black => Direction.down
  }
  def pawnInitialRow(color: Color): Row = color match {
    case White => row2
    case Black => row7
  }
}

object KingValue {
  final val e1 = "e1".toSquare
  final val e8 = "e8".toSquare
  def initialPosition(color: Color): Square = color match {
    case White => e1
    case Black => e8
  }
}

trait Piece {
  def id: PieceId
  def color: Color
  def position: Square

  def shift(direction: Direction): Square = position.shift(direction)
  def letsMove(dest: Square): Piece
  def whereToGo(chessboard: ChessboardImpl)(logBook: LogBook): MovesWithControl
  def display: String
}