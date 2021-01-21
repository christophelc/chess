package config

/**
 * Storage configuration for:
 *  - Piece
 *  - Moves
 *
 *  See [[model.board.ChessboardImpl]]
 */

trait ConfigurationChessboard1 extends ChessboardInitStoragePieceAsSeq with MoveForChessboardAsSeq
trait ConfigurationChessboard2 extends ChessboardInitStoragePieceAsSeq with MoveForChessboardAsMap
trait ConfigurationChessboard3 extends ChessboardInitStoragePieceAsMap with MoveForChessboardAsSeq
trait ConfigurationChessboard4 extends ChessboardInitStoragePieceAsMap with MoveForChessboardAsMap
trait ConfigurationChessboard5 extends ChessboardInitStoragePieceAsBoard with MoveForChessboardAsSeq
trait ConfigurationChessboard6 extends ChessboardInitStoragePieceAsBoard with MoveForChessboardAsMap

///////////////////////////////////////////////
//               |         Move Storage
//               | Move as Seq  | Move as Map |
//---------------|--------------|-------------|
// Piece Storage |              |             |
//---------------|              |             |
// Piece as Seq  |      1       |      2      |
// Piece as Map  |      3       |      4      |
// Piece as Board|      5       |      6      |
//
//  Stat     play move / s   |  number of position /s
//  1            0.17               2500
//  2            0.165              2550
//  3
//  4
//  5
//  6