package config

import model.Chessboard.MovesStorage
import model.Piece.PieceId
import model.{ ChessboardInit, GenericMove, Piece, Pieces }
import model.data.{ PiecesInitStoragePieceBoard, PiecesInitStoragePieceMap, PiecesInitStoragePieceSeq, PiecesStoragePiece, StorageMap, StorageSeq }

trait Configuration

//////////////////////////////
// Move Storage
trait ConfigurationMoveStorage {
  def emptyMove: MovesStorage
}

trait ConfigurationMoveStorageAsMap extends ConfigurationMoveStorage {
  override def emptyMove: MovesStorage = StorageMap.EmptyMoveStorage
}

trait ConfigurationMoveStorageAsSeqPiece extends ConfigurationMoveStorage {
  override def emptyMove: MovesStorage = StorageSeq[Piece, GenericMove](partition = (move: GenericMove) => move.piece)
}

// TODO: Chessboard.MovesStorage is not yet generic
//trait ConfigurationMoveStorageAsSeqPieceId extends ConfigurationMoveStorage {
//  override def emptyMove: MovesStorage = StorageSeq[PieceId, GenericMove](partition = (move: GenericMove) => move.piece.id)
//}

/////////////////////////////
// Piece: Move Storage

trait PieceMoveStorageAsMap extends ConfigurationMoveStorageAsMap

/////////////////////////////
// Chessboard: Piece Storage

trait ChessboardInitStoragePieceAsSeq extends ChessboardInit {
  object PiecesStorage extends PiecesInitStoragePieceSeq
  final val emptyPieces: Pieces = PiecesStorage.emptyPieces
  def buildPieces(pieces: Seq[Piece]): Pieces = PiecesStorage.buildPieces(pieces)
}

trait ChessboardInitStoragePieceAsMap extends ChessboardInit {
  object PiecesStorage extends PiecesInitStoragePieceMap
  final val emptyPieces: Pieces = PiecesStorage.emptyPieces
  def buildPieces(pieces: Seq[Piece]): Pieces = PiecesStorage.buildPieces(pieces)
}

trait ChessboardInitStoragePieceAsBoard extends ChessboardInit {
  object PiecesStorage extends PiecesInitStoragePieceBoard
  final val emptyPieces: Pieces = PiecesStorage.emptyPieces
  def buildPieces(pieces: Seq[Piece]): Pieces = PiecesStorage.buildPieces(pieces)
}

