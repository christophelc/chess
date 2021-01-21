package model

import model.Chessboard.MovesStorage
import model.Piece.PieceId
import model.data.Storage

trait PiecesInit {
  val EmptyPieces: Pieces
  def buildPieces(pieces: Seq[Piece]): Pieces
}

trait Pieces {
  type Store
  val store: Storage[PieceId, Piece]

  def toSeq: Seq[Piece]
  def count: Int
  def union(pieces: Pieces): Pieces
  def withColor(color: Color): Pieces
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
  def pawns: Pieces
  def rooks: Pieces
  def bishops: Pieces
  def knights: Pieces
  def queens: Pieces
  def king(color: Color): Piece
  def atSquare(square: Square): Option[Piece]
  def add(piece: Piece): Pieces
  def sub(piece: Piece): Pieces
  def sub(pieces: Seq[Piece]): Pieces
  def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage
  def containsSameElementAs(pieces: Pieces): Boolean
}
