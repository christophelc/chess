package model.bitboard

import model.{ Chessboard, Col, GenericMove, Row, Square }

/**
 * @param arrByRows the encoded board per row
 */
case class BitboardCodec(arrByRows: Array[Int] = Array(0, 0, 0, 0, 0, 0, 0, 0)) {
  // Int since not unsigned: 128.toByte == --128 !
  final val binary: Array[Int] = Array(1, 2, 4, 8, 16, 32, 64, 128)
  private def bitRowCol(row: Row, col: Col): BitboardCodec = {
    val bit = binary(col.value)
    row match {
      case Square.row1 => BitboardCodec(Array(bit, 0, 0, 0, 0, 0, 0, 0))
      case Square.row2 => BitboardCodec(Array(0, bit, 0, 0, 0, 0, 0, 0))
      case Square.row3 => BitboardCodec(Array(0, 0, bit, 0, 0, 0, 0, 0))
      case Square.row4 => BitboardCodec(Array(0, 0, 0, bit, 0, 0, 0, 0))
      case Square.row5 => BitboardCodec(Array(0, 0, 0, 0, bit, 0, 0, 0))
      case Square.row6 => BitboardCodec(Array(0, 0, 0, 0, 0, bit, 0, 0))
      case Square.row7 => BitboardCodec(Array(0, 0, 0, 0, 0, 0, bit, 0))
      case Square.row8 => BitboardCodec(Array(0, 0, 0, 0, 0, 0, 0, bit))
      case _ => BitboardCodec()
    }
  }

  override def equals(that: Any): Boolean = that match {
    case BitboardCodec(arr) => arrByRows.toSeq == arr.toSeq
    case _ => false
  }

  def &(bitboard: BitboardCodec): BitboardCodec =
    BitboardCodec(for ((arr1, arr2) <- this.arrByRows.zip(bitboard.arrByRows)) yield arr1 & arr2)

  def |(bitboard: BitboardCodec): BitboardCodec =
    BitboardCodec(for ((arr1, arr2) <- this.arrByRows.zip(bitboard.arrByRows)) yield arr1 | arr2)

  def &~(bitboard: BitboardCodec): BitboardCodec =
    BitboardCodec(for ((arr1, arr2) <- this.arrByRows.zip(bitboard.arrByRows)) yield arr1 & ~arr2)

  def nonEmpty(square: Square): Boolean = {
    val bitset = bitRowCol(square.whichRow, square.whichCol)
    (this & bitset) == bitset
  }

  def isEmpty(square: Square): Boolean = !nonEmpty(square)

  private[bitboard] def addPiece(square: Square): BitboardCodec = {
    val board = bitRowCol(square.whichRow, square.whichCol)
    this | board
  }
  private[bitboard] def removePiece(square: Square): BitboardCodec = {
    val bitset = bitRowCol(square.whichRow, square.whichCol)
    // TODO: remove when tested
    require(nonEmpty(square))
    this &~ bitset
  }

  def movePiece(move: GenericMove): BitboardCodec =
    this
      .removePiece(move.piece.position)
      .addPiece(move.dest)

  private def valueToString(v: Int): String = {
    require(v >= 0 && v < 256)
    (for (bit <- binary) yield {
      if ((v & bit) == 0) "0" else "1"
    }).mkString("")
  }

  override def toString(): String =
    arrByRows.toSeq.map(valueToString).mkString("\n")
}
