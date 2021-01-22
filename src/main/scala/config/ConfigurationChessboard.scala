package config

import model.board.PieceBoardBase

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
  trait CurrentMoveStoragePiece extends ConfigurationMoveStorageAsSeqPiece
  trait ConfigurationCurrentChessboard extends ChessboardInitStoragePieceAsSeq
  trait ConfigurationCurrentPieceBoardPiece extends PieceBoardBase
}