package config

import model.Piece.PieceId
import model.{ GenericMove, Piece }
import model.board.PieceBoardBase
import model.data.Storage

/**
 * Storage configuration for:
 *  - Piece
 *  - Moves
 *
 *  See [[model.board.ChessboardImplConfiguration$Piece]] and [[model.board.PieceBoardBase]]
 */

// CommonMoveStorage:
//    => ConfigurationMoveStorageAsSeq
//    => ConfigurationMoveStorageAsMap
// ConfigurationCurrentChessboard:
//    => ChessboardInitStoragePieceAsSeq
//    => ChessboardInitStoragePieceAsMap
//    => ChessboardInitStoragePieceAsBoard
object ConfigurationChessboard {
  // TODO: make it more generic Storage[K, V]
  type MovesStorage = Storage[Piece, GenericMove]
  //type MovesStorage = Storage[PieceId, GenericMove]
  trait CurrentMoveStoragePiece extends ConfigurationMoveStorageAsSeqPiece
  //trait CurrentMoveStoragePiece extends ConfigurationMoveStorageAsSeqPieceId
  trait ConfigurationCurrentChessboard extends ChessboardInitStoragePieceAsSeq
  trait ConfigurationCurrentPieceBoardPiece extends PieceBoardBase
}