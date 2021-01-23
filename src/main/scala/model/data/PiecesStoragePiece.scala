package model.data

import config.ConfigurationChessboard.CurrentMoveStoragePiece
import config.ConfigurationChessboard.MovesStorage
import model.Piece._
import model._
import model.data.PiecesData.emptyPieceBoardStorage

trait PiecesInitStoragePieceBoard extends PiecesInit[PieceId, Piece] {
  override val emptyPieces: Pieces[PieceId, Piece] =
    PiecesStoragePiece[PieceId, Piece](emptyPieceBoardStorage, (p: Piece) => p.id)

  override def buildPieces(pieces: Seq[Piece]): PiecesStoragePiece[PieceId, Piece] = {
    val groups = pieces.groupBy(_.id)
    PiecesStoragePiece[PieceId, Piece](
      PiecesData(StorePieceBoard(
        rooks = groups.getOrElse(idRook, Nil),
        bishops = groups.getOrElse(idBishop, Nil),
        knights = groups.getOrElse(idKnight, Nil),
        queens = groups.getOrElse(idQueen, Nil),
        kings = groups.getOrElse(idKing, Nil),
        pawns = groups.getOrElse(idPawn, Nil))),
      (p: Piece) => p.id)
  }
}

trait PiecesInitStoragePieceSeq extends PiecesInit[PieceId, Piece] {
  def partition(p: Piece): PieceId = p.id
  val emptyPiecesStorage: Storage[PieceId, Piece] = StorageSeq(partition = partition)

  override val emptyPieces: Pieces[PieceId, Piece] = PiecesStoragePiece(emptyPiecesStorage, (p: Piece) => p.id)
  override def buildPieces(pieces: Seq[Piece]): PiecesStoragePiece[PieceId, Piece] =
    PiecesStoragePiece(StorageSeq(partition, pieces), (p: Piece) => p.id)
}

trait PiecesInitStoragePieceMap extends PiecesInit[PieceId, Piece] {
  override val emptyPieces: Pieces[PieceId, Piece] =
    PiecesStoragePiece(StorageMap(store = Map()), (p: Piece) => p.id)
  override def buildPieces(pieces: Seq[Piece]): PiecesStoragePiece[PieceId, Piece] =
    PiecesStoragePiece(StorageMap(pieces.groupBy(_.id)), (p: Piece) => p.id)
}

case class PiecesStoragePiece[K, V <: Piece](store: Storage[K, V], partition: V => K) extends Pieces[K, V] with CurrentMoveStoragePiece {
  override def clear: Pieces[K, V] = PiecesStoragePiece[K, V](store.clear, partition)
  override def toSeq: Seq[V] = store.toSeq
  override def count: Int = store.countV
  override def union(piecesSeq: Pieces[K, V]): Pieces[K, V] =
    piecesSeq match {
      case piecesAdd: PiecesStoragePiece[K, V] => this.copy(store = piecesAdd.store.add(store))
    }
  override def withColor(color: Color): Pieces[K, V] = this.copy(store.filterV(_.color == color))
  override def isEmpty: Boolean = store.isEmpty
  override def pawns: Pieces[K, V] = this.copy(store = store.filterK(_ == idPawn))
  override def rooks: Pieces[K, V] = this.copy(store = store.filterK(_ == idRook))
  override def bishops: Pieces[K, V] = this.copy(store = store.filterK(_ == idBishop))
  override def knights: Pieces[K, V] = this.copy(store = store.filterK(_ == idKnight))
  override def queens: Pieces[K, V] = this.copy(store = store.filterK(_ == idQueen))
  override def king(color: Color): V = store.filterK(_ == idKing).findV(_.color == color).head
  override def atSquare(square: Square): Option[Piece] = store.findV(_.position == square)
  override def add(piece: V): Pieces[K, V] = this.copy(store = store.add(partition(piece))(Seq(piece)))
  override def sub(piece: V): Pieces[K, V] = this.copy(store =
    store.filterK(_ != piece.id).add(store.filterKandV(_ == piece.id)(_ != piece)))
  override def sub(piecesToRemove: Seq[V]): Pieces[K, V] = {
    piecesToRemove.foldLeft(this: Pieces[K, V])((acc: Pieces[K, V], pieceToRemove) => {
      acc.sub(pieceToRemove)
    })
  }
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage = {
    toSeq.map(_.whereToGo(chessboard = chessboard)(logBook = logBook)(emptyMove))
      .foldLeft(emptyMove)((acc, movesWithControl) => acc.add(movesWithControl))
  }

  override def containsSameElementAs(piecesSeq: Pieces[K, V]): Boolean = piecesSeq match {
    case piecesCompare: PiecesStoragePiece[K, V] => toSeq.toSet == piecesCompare.toSeq.toSet
  }
}
