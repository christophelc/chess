package model.analyze

import actor.StatActor
import actor.StatActor.UpdateStat
import dto.{ ChessboardDto, FENCodec }
import model.{ PlayerComputer, White }
import org.specs2.mutable.Specification
import stat.Intercept

class EngineCheckMatSpec extends Specification {

  // https://www.sparkchess.com/chess-puzzles/roberto-grau-vs-edgar-colle.html

  "Engine check mate" should {
    //    "solve a mat in 9 moves" in {
    //      val codec = new FENCodec()
    //      val fen = "3Q4/5q1k/4ppp1/2Kp1N1B/RR6/3P1r2/4nP1b/3b4 w - - 0 1"
    //      val chessboardDto = ChessboardDto(fen)
    //      val game = codec.decode(chessboardDto).copy(playerWhite =
    //        PlayerComputer(name = "Mat in 9 solver", engine = new EngineCheckMate(8)))
    //      // FIXME: Logbook know which player should play
    //      game.play(whichPlayer = White)
    //      codec.encode(game).encoded must equalTo(fen)
    //    }

    //    "solve a mat in 3" in {
    //      val codec = new FENCodec()
    //      val fen = "3r4/pR2N3/2pkb3/5p2/8/2B5/qP3PPP/4R1K1 w - - 0 1"
    //      val chessboardDto = ChessboardDto(fen)
    //      val game = codec.decode(chessboardDto).copy(playerWhite =
    //        PlayerComputer(name = "Mat in 3 solver", engine = new EngineCheckMate(6)))
    //      // FIXME: Logbook know which player should play
    //      println(game.play(whichPlayer = White).tools.logBook.toString())
    //      codec.encode(game).encoded must equalTo(fen)
    //    }

    // OK
    //    "solve a mat in 2" in {
    //      val codec = new FENCodec()
    //      val fen = "1k5r/pP3ppp/3p2b1/1BN1n3/1Q2P3/P1B5/KP3P1P/7q w - - 0 1"
    //      //val fen = "r2qk2r/pb4pp/1n2Pb2/2B2Q2/p1p5/2P5/2B2PPP/RN2R1K1 w - - 0 1"
    //      val chessboardDto = ChessboardDto(fen)
    //      val game = codec.decode(chessboardDto).copy(playerWhite =
    //        PlayerComputer(name = "Mat in 2 solver", engine = new EngineCheckMate(7)))
    //      // FIXME: Logbook know which player should play
    //      println(game.play(whichPlayer = White).tools.logBook.toString())
    //      codec.encode(game).encoded must equalTo(fen)
    //    }

    // OK
    "solve a mat in 2" in {
      val codec = new FENCodec()
      val fen = "r2qk2r/pb4pp/1n2Pb2/2B2Q2/p1p5/2P5/2B2PPP/RN2R1K1 w - - 0 1"
      val chessboardDto = ChessboardDto(fen)
      val game = codec.decode(chessboardDto).copy(playerWhite =
        PlayerComputer(name = "Mat in 2 solver", engine = new EngineCheckMate(depth = 5) with Intercept))
      // FIXME: Logbook know which player should play
      println(game.play(whichPlayer = White).tools.logBook.toString())
      codec.encode(game).encoded must equalTo(fen)
    }

  }
}
