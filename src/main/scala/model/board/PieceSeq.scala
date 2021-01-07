package model.board

import model._

case class PiecesSeq(pieces: Seq[Piece]) extends Pieces {
  override def list: Seq[Piece] = pieces
  override def count: Int = pieces.length
  override def union(piecesSeq: Pieces): Pieces =
    piecesSeq match {
      case PiecesSeq(piecesToAdd) => this.copy(pieces = this.pieces ++ piecesToAdd)
    }
  override def withColor(color: Color): Pieces =
    this.copy(pieces = pieces.filter(_.color == color))
  override def isEmpty: Boolean = pieces.isEmpty
  override def groupByColor: Map[Color, Pieces] =
    pieces
      .groupBy(_.color)
      .map { case (k, v) => k -> PiecesSeq(v) }
  override def pawns: Pieces = this.copy(pieces =
    pieces.filter(piece => piece match {
      case _: Pawn => true
      case _ => false
    }))
  override def rooks: Pieces = this.copy(pieces =
    pieces.filter(piece => piece match {
      case _: Rook => true
      case _ => false
    }))
  override def bishops: Pieces = this.copy(pieces =
    pieces.filter(piece => piece match {
      case _: Bishop => true
      case _ => false
    }))
  override def knights: Pieces = this.copy(pieces =
    pieces.filter(piece => piece match {
      case _: Knight => true
      case _ => false
    }))
  override def queens: Pieces = this.copy(pieces =
    pieces.filter(piece => piece match {
      case _: Queen => true
      case _ => false
    }))
  override def king(color: Color): Piece =
    pieces.filter(piece => piece.color == color && (piece match {
      case _: King => true
      case _ => false
    })).head

  override def atSquare(square: Square): Option[Piece] =
    pieces.find(_.position == square)
  override def add(piece: Piece): Pieces = this.copy(pieces = pieces :+ piece)
  override def sub(piece: Piece): Pieces = this.copy(pieces = pieces.filter(_ != piece))
  override def sub(piecesToRemove: Seq[Piece]): Pieces = this.copy(pieces = pieces.diff(piecesToRemove))
  override def whereToGo(chessboard: ChessboardImpl)(logBook: LogBook): MovesWithControl =
    pieces.map(_.whereToGo(chessboard = chessboard)(logBook = logBook))
      .foldLeft(MovesWithControlImpl())((acc, movesWithControl) => MovesWithControlImpl.convert(acc.concat(movesWithControl)))
  override def containsSameElementAs(piecesSeq: Pieces): Boolean = piecesSeq match {
    case PiecesSeq(piecesToCompare) => pieces.toSet == piecesToCompare.toSet
  }
}
