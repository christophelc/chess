package model.bitboard

import java.math.BigInteger

import model.{ Square, White }
import model.board.{ KingBoard, Move, SquareXY }
import org.specs2.mutable.Specification

class BitBoardCodecSpec extends Specification {

  def binaryToLong(b: String): Long = new BigInteger(b, 2).longValue()

  "min, max and -1L" should {
    "be comparable to binary" in {
      val min = Long.MinValue
      val max = Long.MaxValue
      val minus1 = -1L
      val minBinary = min.toBinaryString
      val maxBinary = max.toBinaryString + "0"
      val minus1Bin = minus1.toBinaryString
      minBinary shouldEqual "1000000000000000000000000000000000000000000000000000000000000000"
      maxBinary shouldEqual "1111111111111111111111111111111111111111111111111111111111111110"
      minus1Bin shouldEqual "1111111111111111111111111111111111111111111111111111111111111111"

      binaryToLong("1000000000000000000000000000000000000000000000000000000000000000") shouldEqual Long.MinValue
    }
  }

  "Empty bitboard" should {
    "have everything set to 0" in {
      val bitboard = BitboardCodec()
      bitboard.arrByRows shouldEqual (
        Array(0, 0, 0, 0, 0, 0, 0, 0))
    }
  }

  "Empty bitboard + a piece added in (row,col)" should {
    "have a bit set to 1 in position (row, col) = (row1, colA)" in {
      val bitboard = BitboardCodec().addPiece(SquareXY(row = Square.row1, col = Square.colA))
      bitboard.arrByRows shouldEqual (
        Array(1, 0, 0, 0, 0, 0, 0, 0))
    }

    "have a bit set to 1 in position (row, col) = (row2, colA)" in {
      val bitboard = BitboardCodec().addPiece(SquareXY(row = Square.row2, col = Square.colA))
      bitboard.arrByRows shouldEqual (
        Array(0, 1, 0, 0, 0, 0, 0, 0))
    }

    "have a bit set to 1 in position (row, col) = (row2, colH)" in {
      val bitboard = BitboardCodec().addPiece(SquareXY(row = Square.row2, col = Square.colH))
      bitboard.arrByRows shouldEqual (
        Array(0, 128, 0, 0, 0, 0, 0, 0))
    }

    "have a bit set to 1 in position (row, col) = (row2, colH)" in {
      val bitboard = BitboardCodec().addPiece(SquareXY(row = Square.row3, col = Square.colD))
      bitboard.arrByRows shouldEqual (
        Array(0, 0, 8, 0, 0, 0, 0, 0))
    }
  }

  "a board with a piece in C1, removed from C1" should {
    "be empty" in {
      val bitboard = BitboardCodec().addPiece(SquareXY(row = Square.row2, col = Square.colH))
      bitboard.removePiece(SquareXY(row = Square.row2, col = Square.colH)) shouldEqual
        BitboardCodec()
    }
  }

  "a board with a piece in C1, moved to C2" should {
    "have a bit set in C2" in {
      val king = KingBoard(White, SquareXY(Square.row1, Square.colC))
      val bitboard = BitboardCodec().addPiece(king.position)
      val move = Move(
        piece = king,
        dest = SquareXY(Square.row2, Square.colC))
      bitboard.movePiece(move).arrByRows shouldEqual
        Array(0, 4, 0, 0, 0, 0, 0, 0)
    }
  }

  "a board with a piece in C1" should {
    "return false for isEmpty(C1) and true for isEmpty(C2)" in {
      val C1 = SquareXY(row = Square.row1, col = Square.colC)
      val C2 = SquareXY(row = Square.row2, col = Square.colC)
      val bitboard = BitboardCodec().addPiece(C1)
      bitboard.isEmpty(C1) should beFalse
      bitboard.isEmpty(C2) should beTrue
    }
  }

}
