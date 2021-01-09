package dto

import model.board.{ Bishop, ChessboardImpl, King, Knight, Pawn, PiecesSeq, Queen, Rook, SquareXY }
import model.{ Chessboard, _ }

case class Bitmap(colorWithPieceId: Byte, bytes: Seq[Byte])

object CodecBitmap {

  private def encodeColorWithPieceId(color: Color, pieceId: Int): Byte =
    color match {
      case White => pieceId.toByte
      case Black => (pieceId + 6).toByte
    }

  private def decodeColorWithPieceId(byte: Byte): (Color, Int) =
    (if (byte.toInt >= 6) Black else White, byte.toInt % 6)

  def encode(chessboard: Chessboard): Seq[Bitmap] = {
    val groupBy = chessboard.pieces.list
      .groupBy(piece => (piece.color, piece.id))
    groupBy.map {
      case (key, pieces) =>
        Bitmap(
          colorWithPieceId = encodeColorWithPieceId(color = key._1, pieceId = key._2),
          // the order is important for the unicity of the encoding
          bytes = pieces.map(_.position.toInt.toByte).sortBy(identity))
    }.toSeq.sortBy(_.colorWithPieceId)
  }

  // TODO: small / great castling
  def decode(bitmaps: Seq[Bitmap]): Chessboard = {
    val pieces = bitmaps.flatMap {
      case Bitmap(colorWithPieceId, bytes) =>
        val (color, pieceId) = decodeColorWithPieceId(colorWithPieceId)
        val squares = bytes.map(_.toInt)
        squares.map(i => {
          val square = SquareXY(row = (i / 8).toByte, col = (i % 8).toByte)
          pieceId match {
            case Piece.idBishop => Bishop(color, square)
            case Piece.idKnight => Knight(color, square)
            case Piece.idKing => King(color, square)
            case Piece.idQueen => Queen(color, square)
            case Piece.idRook => Rook(color, square)
            case Piece.idPawn => Pawn(color, square)
          }
        })
    }
    ChessboardImpl(PiecesSeq(pieces))
  }
}

case class Bitmaps(bytes: Seq[Byte])

object CodecBitmaps {
  def encode(chessboard: Chessboard): Bitmaps = {
    val groupBy = CodecBitmap.encode(chessboard).groupBy(b => b.colorWithPieceId.toInt)
    val bytes = Range(0, 12).flatMap(i => {
      val n = groupBy.get(i) match {
        case None => 0.toByte
        case Some(seq) => seq.head.bytes.size.toByte
      }
      val s = groupBy.getOrElse(i, Nil).flatMap(x => x.bytes)
      n +: s
    }).toSeq
    Bitmaps(bytes)
  }

  def decode(bitmaps: Bitmaps): Chessboard = {
    case class Acc(id: Int, skip: Int, seq: Seq[Byte] = Nil)

    val ranges = bitmaps.bytes.foldLeft(Nil: Seq[Acc])(
      (seq, byte) => seq match {
        case Nil => Seq(Acc(id = 0, skip = byte.toInt))
        case seq => {
          val acc = seq.last
          if (acc.skip > 0) {
            seq.dropRight(1) :+ acc.copy(skip = acc.skip - 1, seq = acc.seq :+ byte)
          } else {
            seq :+ Acc(id = acc.id + 1, skip = byte.toInt)
          }
        }
      })
    CodecBitmap.decode(ranges.map(acc => Bitmap(acc.id.toByte, acc.seq)))
  }
}
