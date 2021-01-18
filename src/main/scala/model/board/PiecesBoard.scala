package model.board

import model.Chessboard.MovesStorage
import model.Piece._
import model._
import model.board.StorageImpl.emptyMoveStorage

object PiecesBoard {
  def build(pieces: Seq[Piece]): PiecesBoard = {
    val groups = pieces.groupBy(_.id)
    PiecesBoard(
      _rooks = groups.getOrElse(idRook, Nil),
      _bishops = groups.getOrElse(idBishop, Nil),
      _knights = groups.getOrElse(idKnight, Nil),
      _queens = groups.getOrElse(idQueen, Nil),
      _kings = groups.getOrElse(idKing, Nil),
      _pawns = groups.getOrElse(idPawn, Nil))
  }
}

case class PiecesBoard(
  _rooks: Seq[Piece] = Nil,
  _bishops: Seq[Piece] = Nil,
  _knights: Seq[Piece] = Nil,
  _kings: Seq[Piece] = Nil,
  _queens: Seq[Piece] = Nil,
  _pawns: Seq[Piece] = Nil) extends Pieces {

  private def all: Seq[Seq[Piece]] =
    Seq(_rooks, _bishops, _knights, _kings, _queens, _pawns)
  override def toSeq: Seq[Piece] = all.flatten
  override def count: Int = all.map(_.size).sum
  override def union(piecesSeq: Pieces): Pieces =
    piecesSeq match {
      case piecesAdd: PiecesBoard => this.copy(
        _rooks = _rooks ++ piecesAdd._rooks,
        _bishops = _bishops ++ piecesAdd._bishops,
        _knights = _knights ++ piecesAdd._knights,
        _kings = _kings ++ piecesAdd._kings,
        _queens = _queens ++ piecesAdd._queens,
        _pawns = _pawns ++ piecesAdd._pawns)
    }
  override def withColor(color: Color): Pieces =
    this.copy(
      _rooks = _rooks.filter(_.color == color),
      _bishops = _bishops.filter(_.color == color),
      _knights = _knights.filter(_.color == color),
      _kings = _kings.filter(_.color == color),
      _queens = _queens.filter(_.color == color),
      _pawns = _pawns.filter(_.color == color))

  override def isEmpty: Boolean = all.forall(_.isEmpty)
  override def pawns: Pieces = PiecesBoard(_pawns)
  override def rooks: Pieces = PiecesBoard(_rooks)
  override def bishops: Pieces = PiecesBoard(_bishops)
  override def knights: Pieces = PiecesBoard(_knights)
  override def queens: Pieces = PiecesBoard(_queens)
  override def king(color: Color): Piece =
    _kings
      .filter(_.color == color)
      .head
  override def atSquare(square: Square): Option[Piece] =
    toSeq.find(_.position == square)
  override def add(piece: Piece): Pieces = piece.id match {
    case Piece.idRook => this.copy(_rooks = _rooks :+ piece)
    case Piece.idKnight => this.copy(_knights = _knights :+ piece)
    case Piece.idBishop => this.copy(_bishops = _bishops :+ piece)
    case Piece.idQueen => this.copy(_queens = _queens :+ piece)
    case Piece.idKing => this.copy(_kings = _kings :+ piece)
    case Piece.idPawn => this.copy(_pawns = _pawns :+ piece)
  }
  override def sub(piece: Piece): Pieces = piece.id match {
    case Piece.idRook => this.copy(_rooks = _rooks.filter(_ != piece))
    case Piece.idKnight => this.copy(_knights = _knights.filter(_ != piece))
    case Piece.idBishop => this.copy(_bishops = _bishops.filter(_ != piece))
    case Piece.idQueen => this.copy(_queens = _queens.filter(_ != piece))
    case Piece.idKing => this.copy(_kings = _kings.filter(_ != piece))
    case Piece.idPawn => this.copy(_pawns = _pawns.filter(_ != piece))
  }
  override def sub(piecesToRemove: Seq[Piece]): Pieces = {
    piecesToRemove.foldLeft(this: Pieces)((acc: Pieces, pieceToRemove) => {
      acc.sub(pieceToRemove)
    })
  }
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage =
    toSeq.map(_.whereToGo(chessboard = chessboard)(logBook = logBook))
      .foldLeft(emptyMoveStorage)((acc, movesWithControl) => acc.add(movesWithControl))

  override def containsSameElementAs(piecesSeq: Pieces): Boolean = piecesSeq match {
    case piecesCompare: PiecesBoard => toSeq.toSet == piecesCompare.toSeq.toSet
  }
}
