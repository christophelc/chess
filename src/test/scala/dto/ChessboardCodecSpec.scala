package dto

import org.specs2.mutable.Specification
import model.{ Chessboard, _ }
import model.board.RichSquare._
import model.board.ChessboardImplConfiguration$Piece

class ChessboardCodecSpec extends Specification {

  "Bitmap encoding chessboard" should {
    "be decoded" in {
      val chessboard: Chessboard = ChessboardImplConfiguration$Piece()
      val encode = CodecBitmap.encode(chessboard)
      val chessboard2: Chessboard = CodecBitmap.decode(encode)
      chessboard2 shouldEqual chessboard
      // check the order too
      encode should beEqualTo(Seq(
        Bitmap(0, Seq(3)),
        Bitmap(1, Seq(4)),
        Bitmap(2, Seq(2, 5)),
        Bitmap(3, Seq(1, 6)),
        Bitmap(4, Seq(0, 7)),
        Bitmap(5, Seq(8, 9, 10, 11, 12, 13, 14, 15)),
        Bitmap(6, Seq(59)),
        Bitmap(7, Seq(60)),
        Bitmap(8, Seq(58, 61)),
        Bitmap(9, List(57, 62)),
        Bitmap(10, Seq(56, 63)),
        Bitmap(11, Seq(48, 49, 50, 51, 52, 53, 54, 55))))
    }
  }

  "Bitmaps encoding chessboard" should {
    "be decoded" in {
      val chessboard: Chessboard = ChessboardImplConfiguration$Piece()
      val encode = CodecBitmaps.encode(chessboard)
      val chessboard2: Chessboard = CodecBitmaps.decode(encode)
      chessboard2 shouldEqual chessboard
    }
  }
}
