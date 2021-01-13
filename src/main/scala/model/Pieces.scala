package model

trait Pieces {
  def list: Seq[Piece]
  def count: Int
  def union(pieces: Pieces): Pieces
  def withColor(color: Color): Pieces
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
  def groupByColor: Map[Color, Pieces]
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
  def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesWithControl
  def containsSameElementAs(pieces: Pieces): Boolean
}
