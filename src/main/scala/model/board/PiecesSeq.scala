package model.board

import model.Chessboard.MovesStorage
import model.Piece._
import model._
import model.board.PiecesSeq.PiecesAsSeq
import model.board.StorageImpl.emptyMoveStorage

object PiecesSeq {
  type PiecesAsSeq = Storage[PieceId, Piece]
  val EmptyPieces: PiecesSeq = PiecesSeq(Nil)
  def build(pieces: Seq[Piece]): PiecesSeq = PiecesSeq(pieces)
}

case class PiecesSeq(pieces: Seq[Piece]) extends Pieces {
  override def toSeq: Seq[Piece] = pieces
  override def count: Int = pieces.length
  override def union(piecesSeq: Pieces): Pieces =
    piecesSeq match {
      case PiecesSeq(piecesToAdd) => this.copy(pieces = this.pieces ++ piecesToAdd)
    }
  override def withColor(color: Color): Pieces =
    this.copy(pieces = pieces.filter(_.color == color))
  override def isEmpty: Boolean = pieces.isEmpty
  //  override def groupByColor: Map[Color, Pieces] =
  //    pieces
  //      .toSeq
  //      .groupBy(_.color)
  //      .map { case (k, seqV) => k -> seqV }
  override def pawns: Pieces = this.copy(pieces =
    pieces.filter(_.id == idPawn))

  override def rooks: Pieces = this.copy(pieces =
    pieces.filter(_.id == idRook))

  override def bishops: Pieces = this.copy(pieces =
    pieces.filter(_.id == idBishop))

  override def knights: Pieces = this.copy(pieces =
    pieces.filter(_.id == idKnight))

  override def queens: Pieces = this.copy(pieces =
    pieces.filter(_.id == idQueen))

  override def king(color: Color): Piece =
    pieces
      .filter(piece => piece.color == color && piece.id == idKing)
      .head
  override def atSquare(square: Square): Option[Piece] =
    pieces.find(_.position == square)
  override def add(piece: Piece): Pieces = this.copy(pieces = pieces :+ piece)
  override def sub(piece: Piece): Pieces = this.copy(pieces = pieces.filter(_ != piece))
  override def sub(piecesToRemove: Seq[Piece]): Pieces = this.copy(pieces = pieces.diff(piecesToRemove))
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage =
    pieces.map(_.whereToGo(chessboard = chessboard)(logBook = logBook))
      .foldLeft(emptyMoveStorage)((acc, movesWithControl) => acc.add(movesWithControl))

  override def containsSameElementAs(piecesSeq: Pieces): Boolean = piecesSeq match {
    case PiecesSeq(piecesToCompare) => pieces.toSet == piecesToCompare.toSet
  }
}
