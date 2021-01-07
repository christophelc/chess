package model.bitboard

import model.Piece
import model.bitboard.PieceBitboard.emptyBitboard

import scala.collection.BitSet

object PieceBitboard {
  type PieceCodec = BitSet
  val emptyBitboard: PieceCodec = BitSet(0, 0, 0, 0, 0, 0, 0, 0)

}

import PieceBitboard._
//case class PieceBitboard(b: PieceCodec = emptyBitboard) extends Piece {
//
//}
