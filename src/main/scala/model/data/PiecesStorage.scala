package model.data

import model.Chessboard.MovesStorage
import model.Piece._
import model._
import model.data.StorageMap.EmptyMoveStorage
import model.data.PiecesData.emptyPieceBoardStorage

trait PiecesInitStoragePieceBoard extends PiecesInit {
  override val EmptyPieces: Pieces = PiecesStorage(emptyPieceBoardStorage)

  override def build(pieces: Seq[Piece]): PiecesStorage = {
    val groups = pieces.groupBy(_.id)
    PiecesStorage(PiecesData(StorePieceBoard(
      rooks = groups.getOrElse(idRook, Nil),
      bishops = groups.getOrElse(idBishop, Nil),
      knights = groups.getOrElse(idKnight, Nil),
      queens = groups.getOrElse(idQueen, Nil),
      kings = groups.getOrElse(idKing, Nil),
      pawns = groups.getOrElse(idPawn, Nil))))
  }
}

trait PiecesInitStoragePieceSeq extends PiecesInit {
  override val EmptyPieces: Pieces = PiecesStorage(StoragePieceSeq(store = Nil))
  override def build(pieces: Seq[Piece]): PiecesStorage = PiecesStorage(StoragePieceSeq(pieces))
}

trait PiecesInitStoragePieceMap extends PiecesInit {
  override val EmptyPieces: Pieces = PiecesStorage(StorageMap(store = Map()))
  override def build(pieces: Seq[Piece]): PiecesStorage = PiecesStorage(StorageMap(
    pieces.groupBy(_.id)))
}

//object PiecesStorage extends PiecesInitStoragePieceBoard
object PiecesStorage extends PiecesInitStoragePieceSeq
//object PiecesStorage extends PiecesInitStoragePieceMap

case class PiecesStorage(store: Storage[PieceId, Piece]) extends Pieces {

  override def toSeq: Seq[Piece] = store.toSeq
  override def count: Int = store.countV
  override def union(piecesSeq: Pieces): Pieces =
    piecesSeq match {
      case piecesAdd: PiecesStorage => this.copy(store = piecesAdd.store.add(store))
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
  // TODO: optimize filter (K, V) avoid iteration over all the pieces
  override def sub(piece: Piece): Pieces = this.copy(store = store.filterV(_ != piece))
  override def sub(piecesToRemove: Seq[Piece]): Pieces = {
    piecesToRemove.foldLeft(this: Pieces)((acc: Pieces, pieceToRemove) => {
      acc.sub(pieceToRemove)
    })
  }
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage =
    toSeq.map(_.whereToGo(chessboard = chessboard)(logBook = logBook))
      .foldLeft(EmptyMoveStorage)((acc, movesWithControl) => acc.add(movesWithControl))

  override def containsSameElementAs(piecesSeq: Pieces): Boolean = piecesSeq match {
    case piecesCompare: PiecesStorage => toSeq.toSet == piecesCompare.toSeq.toSet
  }
}
