package config

/**
 * Storage configuration for:
 *  - Piece
 *  - Moves
 *
 *  See [[model.board.ChessboardImpl]]
 */

trait ConfigurationChessboard3 extends ChessboardInitStoragePieceAsSeq with MoveForChessboardAsSeq

trait ConfigurationChessboard4 extends ChessboardInitStoragePieceAsSeq with MoveForChessboardAsMap

///////////////////////////////////////////////
// Move Storage  | Move as Seq  | Move as Map |
//               |              |             |
// Piece Storage |      1       |      2      |
//               |              |             |
// Piece as Seq  |      3       |      4      |
// Piece as Map  |      5       |      6      |
// Piece as Board|      7       |      8      |
//