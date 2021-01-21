package config

import model.Chessboard.MovesStorage
import model.{ ChessboardInit, GenericMove, Piece, Pieces }
import model.data.{ PiecesInitStoragePieceBoard, PiecesInitStoragePieceMap, PiecesInitStoragePieceSeq, PiecesStorage, StorageMap, StorageSeq }

trait Configuration

//////////////////////////////
// Move Storage
trait MoveStorageForChessboard {
  val emptyMove: MovesStorage
}

trait MoveForChessboardAsMap extends MoveStorageForChessboard {
  override val emptyMove: MovesStorage = StorageMap.EmptyMoveStorage
}

trait MoveForChessboardAsSeq extends MoveStorageForChessboard {
  def partition(move: GenericMove): Piece = move.piece
  override val emptyMove: MovesStorage = StorageSeq[Piece, GenericMove](partition = partition)
}

//////////////////////////////
// Piece Storage

trait ChessboardInitStoragePieceAsSeq extends ChessboardInit {
  object PiecesStorage extends PiecesInitStoragePieceSeq
  val emptyPieces: Pieces = PiecesStorage.EmptyPieces
  def buildPieces(pieces: Seq[Piece]): Pieces = PiecesStorage.buildPieces(pieces)
}

trait ChessboardInitStoragePieceAsMap extends ChessboardInit {
  object PiecesStorage extends PiecesInitStoragePieceMap
  val emptyPieces: Pieces = PiecesStorage.EmptyPieces
  def buildPieces(pieces: Seq[Piece]): Pieces = PiecesStorage.buildPieces(pieces)
}

trait ChessboardInitStoragePieceAsBoard extends ChessboardInit {
  object PiecesStorage extends PiecesInitStoragePieceBoard
  val emptyPieces: Pieces = PiecesStorage.EmptyPieces
  def buildPieces(pieces: Seq[Piece]): Pieces = PiecesStorage.buildPieces(pieces)
}

