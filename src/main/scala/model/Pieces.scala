package model

import config.ConfigurationChessboard.MovesStorage
import model.Piece.PieceId
import model.data.Storage

trait PiecesInit[K, V <: Piece] {
  def emptyPieces: Pieces[K, V]
  def buildPieces(pieces: Seq[Piece]): Pieces[K, V]
}

trait Pieces[K, V <: Piece] {
  val store: Storage[K, V]

  def clear: Pieces[K, V]
  def emptyMove: MovesStorage
  def toSeq: Seq[Piece]
  def count: Int
  def union(pieces: Pieces[K, V]): Pieces[K, V]
  def withColor(color: Color): Pieces[K, V]
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
  def pawns: Pieces[K, V]
  def rooks: Pieces[K, V]
  def bishops: Pieces[K, V]
  def knights: Pieces[K, V]
  def queens: Pieces[K, V]
  def king(color: Color): Piece
  def atSquare(square: Square): Option[Piece]
  def add(piece: V): Pieces[K, V]
  def sub(piece: V): Pieces[K, V]
  def sub(pieces: Seq[V]): Pieces[K, V]
  def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage
  def containsSameElementAs(pieces: Pieces[K, V]): Boolean
}
