package model

import model.Chessboard.MovesStorage
import model.Piece._
import model.Square.{ row2, row7 }

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

trait Piece {
  def id: PieceId
  def color: Color
  def position: Square

  def emptyMove: MovesStorage
  def shift(direction: Direction): Square = position.shift(direction)
  def letsMove(dest: Square): Piece
  def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage
  def display: String
}

trait King extends Piece {
  override def id = idKing
  override def display: String = if (color == Black) "♔" else "♚"
}
trait Queen extends Piece {
  override def id = idQueen
  override def display: String = if (color == Black) "♕" else "♛"
}
trait Rook extends Piece {
  override def id = idRook
  override def display: String = if (color == Black) "♖" else "♜"
}
trait Bishop extends Piece {
  override def id = idBishop
  override def display: String = if (color == Black) "♗" else "♝"
}
trait Knight extends Piece {
  override def id = idKnight
  override def display: String = if (color == Black) "♘" else "♞"
}
trait Pawn extends Piece {
  override def id = idPawn
}