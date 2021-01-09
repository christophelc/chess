package dto

import org.specs2.mutable.Specification
import model.{ board, _ }
import model.board.RichSquare._
import model.board.{ ChessboardImpl, King, PiecesSeq }

class ChessboardDtoSpec extends Specification {

  "KA1;KA8" should {
    "generate a chessboard with White King in A1 and black king in A8" in {
      val codec = new SimpleChessboardCodec()
      val chessboardDto = ChessboardDto("KA1" + SimpleChessboardCodec.colorSeparator + "KA8")
      val chessboard = codec.decode(chessboardDto)
      chessboard must equalTo(ChessboardImpl(pieces = PiecesSeq(
        Seq(
          King(White, "a1".toSquare),
          board.King(Black, "a8".toSquare)))))
    }
  }

  "Full chessboard string representing a starting game" should {
    "generate a chessboard with a starting game" in {
      val codec = new SimpleChessboardCodec()
      val init = "Bc1Bf1Ke1Nb1Ng1Qd1Ra1Rh1pa2pb2pc2pd2pe2pf2pg2ph2" +
        SimpleChessboardCodec.colorSeparator +
        "Bc8Bf8Ke8Nb8Ng8Qd8Ra8Rh8pa7pb7pc7pd7pe7pf7pg7ph7"
      val chessboardDto = ChessboardDto(init)
      val chessboard = codec.decode(chessboardDto)
      codec.encode(chessboard).encoded must equalTo(init)
      chessboard must equalTo(ChessboardImpl())
    }
  }

  "FEN" should {
    "generate a starting new chessboard game" in {
      val codec = new FENCodec()
      val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      val chessboardDto = ChessboardDto(fen)
      val game = codec.decode(chessboardDto)
      codec.encode(game).encoded must equalTo(fen)
      game.tools.chessboard must equalTo(ChessboardImpl())
    }

    "generate a mat in 9 position" in {
      val codec = new FENCodec()
      val fen = "3Q4/5q1k/4ppp1/2Kp1N1B/RR6/3P1r2/4nP1b/3b4 w - - 0 1"
      val chessboardDto = ChessboardDto(fen)
      val game = codec.decode(chessboardDto)
      codec.encode(game).encoded must equalTo(fen)
    }
  }

}
