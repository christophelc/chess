package model.data

import config.ConfigurationChessboard.CurrentMoveStoragePiece
import model.Chessboard.MovesStorage
import model.Piece._
import model._
import model.data.PiecesData.emptyPieceBoardStorage

trait PiecesInitStoragePieceBoard extends PiecesInit {
  override val emptyPieces: Pieces = PiecesStoragePiece(emptyPieceBoardStorage)

  override def buildPieces(pieces: Seq[Piece]): PiecesStoragePiece = {
    val groups = pieces.groupBy(_.id)
    PiecesStoragePiece(PiecesData(StorePieceBoard(
      rooks = groups.getOrElse(idRook, Nil),
      bishops = groups.getOrElse(idBishop, Nil),
      knights = groups.getOrElse(idKnight, Nil),
      queens = groups.getOrElse(idQueen, Nil),
      kings = groups.getOrElse(idKing, Nil),
      pawns = groups.getOrElse(idPawn, Nil))))
  }
}

trait PiecesInitStoragePieceSeq extends PiecesInit {
  def partition(p: Piece): PieceId = p.id
  val emptyPiecesStorage: Storage[PieceId, Piece] = StorageSeq(partition = partition)

  override val emptyPieces: Pieces = PiecesStoragePiece(emptyPiecesStorage)
  override def buildPieces(pieces: Seq[Piece]): PiecesStoragePiece = PiecesStoragePiece(StorageSeq(partition, pieces))
}

trait PiecesInitStoragePieceMap extends PiecesInit {
  override val emptyPieces: Pieces = PiecesStoragePiece(StorageMap(store = Map()))
  override def buildPieces(pieces: Seq[Piece]): PiecesStoragePiece = PiecesStoragePiece(StorageMap(
    pieces.groupBy(_.id)))
}

case class PiecesStoragePiece(store: Storage[PieceId, Piece]) extends Pieces with CurrentMoveStoragePiece {
  override type Store = Storage[PieceId, Piece]

  override def toSeq: Seq[Piece] = store.toSeq
  override def count: Int = store.countV
  override def union(piecesSeq: Pieces): Pieces =
    piecesSeq match {
      case piecesAdd: PiecesStoragePiece => this.copy(store = piecesAdd.store.add(store))
    }
  override def withColor(color: Color): Pieces = this.copy(store.filterV(_.color == color))
  override def isEmpty: Boolean = store.isEmpty
  override def pawns: Pieces = this.copy(store = store.filterK(_ == idPawn))
  override def rooks: Pieces = this.copy(store = store.filterK(_ == idRook))
  override def bishops: Pieces = this.copy(store = store.filterK(_ == idBishop))
  override def knights: Pieces = this.copy(store = store.filterK(_ == idKnight))
  override def queens: Pieces = this.copy(store = store.filterK(_ == idQueen))
  override def king(color: Color): Piece = store.filterK(_ == idKing).findV(_.color == color).head
  override def atSquare(square: Square): Option[Piece] = store.findV(_.position == square)
  override def add(piece: Piece): Pieces = this.copy(store = store.add(piece.id)(Seq(piece)))
  override def sub(piece: Piece): Pieces = this.copy(store =
    store.filterK(_ != piece.id).add(store.filterKandV(_ == piece.id)(_ != piece)))
  override def sub(piecesToRemove: Seq[Piece]): Pieces = {
    piecesToRemove.foldLeft(this: Pieces)((acc: Pieces, pieceToRemove) => {
      acc.sub(pieceToRemove)
    })
  }
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage = {
    // retrieve the MoveStorage used by pieces and clear to be sure to use the same type
    // FIXME: not totally satisfying. We should be sure that MoveStorage is the same for
    // Chessboard, Pieces and each subtype of Piece
    val pieces = toSeq
    pieces.map(_.whereToGo(chessboard = chessboard)(logBook = logBook))
      .foldLeft(pieces.head.emptyMove)((acc, movesWithControl) => acc.add(movesWithControl))
  }

  override def containsSameElementAs(piecesSeq: Pieces): Boolean = piecesSeq match {
    case piecesCompare: PiecesStoragePiece => toSeq.toSet == piecesCompare.toSeq.toSet
  }
}
