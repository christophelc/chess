package model.bitboard

import model.Chessboard

import scala.collection.BitSet

object Bitboard {
  type PieceCodec = BitSet
  val emptyBitboard: PieceCodec = BitSet(0, 0, 0, 0, 0, 0, 0, 0)

}

import Bitboard._
//case class Bitboard(b: PieceCodec = emptyBitboard)

//case class BitChessboard(
//  white: PieceCodec = emptyBitboard,
//  black: PieceCodec = emptyBitboard,
//  rooks: PieceCodec = emptyBitboard,
//  bishops: PieceCodec = emptyBitboard,
//  knights: PieceCodec = emptyBitboard,
//  queens: PieceCodec = emptyBitboard,
//  kings: PieceCodec = emptyBitboard) extends Chessboard {
//
//  override val controls: Moves = EmptyMove
//  override val endGame: Option[EndGame] = None
//
//  override def pieces: Pieces
//  override def findKing(color: Color): Piece
//  override def get(square: Square): Option[Piece] = pieces.atSquare(square)
//  override def isAttackedByColor(square: Square, color: Color): Boolean
//  override def updateControls(logBook: LogBook): Chessboard
//  override def play(move: GenericMove): Chessboard
//  override def clear(square: Square): Chessboard
//  override def +(piece: Piece): Chessboard
//  override def generateMoveWithControl(color: Color)(logBook: LogBook): MovesWithControl
//  override def generateMove(color: Color)(logBook: LogBook): Moves
//  override def withEndGame(endGame: Option[EndGame]): Chessboard
//
//}
