package model

import org.specs2.mutable.Specification
import model.board.RichSquare._
import model.board._

class PieceSpec extends Specification {

  val logBook: LogBook = LogBook()

  def generateMoveWithControl(tools: Tools, color: Color): MovesWithControlImpl =
    MovesWithControlImpl.convert(tools.chessboard.generateMoveWithControl(color)(tools.logBook))

  "A King in a1" should {
    "move to a2 and b1" in {
      val king = KingBoardImpl(White, "a1".toSquare)
      val chessboard = ChessboardImpl.empty + king
      generateMoveWithControl(Tools(chessboard, logBook), White).moves.toSeq should containTheSameElementsAs(
        Seq("a2", "b1", "b2").map(s => Move(king, s.toSquare)))
    }
  }

  "A King in c2" should {
    "move to b3, c3, d3, b2, b1, c1, d1, d2" in {
      val king = board.KingBoardImpl(White, "c2".toSquare)
      val chessboard = ChessboardImpl.empty + king
      generateMoveWithControl(Tools(chessboard, logBook), White).moves.toSeq should containTheSameElementsAs(
        Seq("b3", "c3", "d3", "b2", "b1", "c1", "d1", "d2").map(s => Move(king, s.toSquare)))
    }
  }

  "A King in e1 with its rook in h1" should {
    "be able to do small castling" in {
      val king = board.KingBoardImpl(White, "e1".toSquare)
      val rook = RookBoardImpl(White, "h1".toSquare)
      val chessboard = ChessboardImpl.empty + king + rook
      generateMoveWithControl(Tools(chessboard, logBook), White).controls
        .filterPiece(king).toSeq
        .exists(_.dest == "g1".toSquare) should beFalse
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(king).toSeq
        .exists(_.dest == "g1".toSquare) should beTrue
    }
  }

  "A King in e1 with its rook in h1 but a piece in g1" should {
    "not be able to do small castling" in {
      val king = board.KingBoardImpl(White, "e1".toSquare)
      val rook = board.RookBoardImpl(White, "h1".toSquare)
      val knight = KnightBoardImpl(White, "g1".toSquare)
      val chessboard = ChessboardImpl.empty + king + rook + knight
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(king).toSeq
        .exists(_.dest == "g1".toSquare) should beFalse
    }
  }

  "A King in e1 with its rook in h1 but a piece in f1" should {
    "not be able to do small castling" in {
      val king = board.KingBoardImpl(White, "e1".toSquare)
      val rook = board.RookBoardImpl(White, "h1".toSquare)
      val knight = BishopBoardImpl(White, "f1".toSquare)
      val chessboard = ChessboardImpl.empty + king + rook + knight
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(king).toSeq
        .exists(_.dest == "g1".toSquare) should beFalse
    }
  }

  "A King in e1 with its rook in a1" should {
    "be able to do great castling" in {
      val king = board.KingBoardImpl(White, "e1".toSquare)
      val rook = board.RookBoardImpl(White, "a1".toSquare)
      val chessboard = ChessboardImpl.empty + king + rook
      generateMoveWithControl(Tools(chessboard, logBook), White).controls
        .filterPiece(king).toSeq
        .exists(_.dest == "c1".toSquare) should beFalse
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(king).toSeq
        .exists(_.dest == "c1".toSquare) should beTrue
    }
  }

  "A King in e1 with its rook in a1 but a piece in b1" should {
    "not be able to do great castling" in {
      val king = board.KingBoardImpl(White, "e1".toSquare)
      val rook = board.RookBoardImpl(White, "a1".toSquare)
      val knight = board.KnightBoardImpl(White, "b1".toSquare)
      val chessboard = ChessboardImpl.empty + king + rook + knight
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(king).toSeq
        .exists(_.dest == "c1".toSquare) should beFalse
    }
  }

  "A King in e1 with its rook in a1 but a piece in d1" should {
    "not be able to do great castling" in {
      val king = board.KingBoardImpl(White, "e1".toSquare)
      val rook = board.RookBoardImpl(White, "a1".toSquare)
      val queen = QueenBoardImpl(White, "d1".toSquare)
      val chessboard = ChessboardImpl.empty + king + rook + queen
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(king).toSeq
        .exists(_.dest == "c1".toSquare) should beFalse
    }
  }

  "A Bishop in c2 with 2 pawns" should {
    "move to d1, b3, a4, b1, d3, e4, f5, g6, h7" in {
      val bishop = board.BishopBoardImpl(White, "c2".toSquare)
      val chessboard = ChessboardImpl.empty + bishop
      generateMoveWithControl(Tools(chessboard, logBook), White).moves.toSeq should containTheSameElementsAs(
        Seq("d1", "b3", "a4", "b1", "d3", "e4", "f5", "g6", "h7").map(s => Move(bishop, s.toSquare)))
    }
  }

  "A Bishop in c2" should {
    "move to d1, b3, a4, b1, d3, e4, f5" in {
      val bishop = board.BishopBoardImpl(White, "c2".toSquare)
      val pawnG6 = PawnBoardImpl(White, "g6".toSquare)
      val pawnB3 = board.PawnBoardImpl(Black, "b3".toSquare)
      val chessboard = ChessboardImpl.empty + bishop + pawnG6 + pawnB3
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(bishop).toSeq.map(_.dest) should containTheSameElementsAs(
          Seq("d1", "b3", "b1", "d3", "e4", "f5").map(s => s.toSquare))
    }
  }

  "A Rook in c2" should {
    "move to a2, b2, c2, d2, e2, f2, c1, c3, c4, c5, c6, c7, c8" in {
      val rook = board.RookBoardImpl(White, "c2".toSquare)
      val chessboard = ChessboardImpl.empty + rook
      generateMoveWithControl(Tools(chessboard, logBook), White).moves.toSeq should containTheSameElementsAs(
        Seq("a2", "b2", "d2", "e2", "f2", "g2", "h2", "c1", "c3", "c4", "c5", "c6", "c7", "c8").map(s => Move(rook, s.toSquare)))
    }
  }

  "A Rook in c2 with 2 pawns" should {
    "move to a2, b2, c2, d2, e2, f2, c1, c3, c4, c5, c6, c7, c8" in {
      val rook = board.RookBoardImpl(White, "c2".toSquare)
      val pawnC6 = board.PawnBoardImpl(White, "c6".toSquare)
      val pawnG2 = board.PawnBoardImpl(Black, "g2".toSquare)
      val chessboard = ChessboardImpl.empty + rook + pawnC6 + pawnG2
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(rook).toSeq.map(_.dest) should containTheSameElementsAs(
          Seq("a2", "b2", "d2", "e2", "f2", "g2", "c1", "c3", "c4", "c5").map(s => s.toSquare))
    }
  }

  "A Queen in c2" should {
    "move to all the directions toward the chessboard" in {
      val queen = board.QueenBoardImpl(White, "c2".toSquare)
      val chessboard = ChessboardImpl.empty + queen
      generateMoveWithControl(Tools(chessboard, logBook), White).moves.toSeq should containTheSameElementsAs(
        Seq("d1", "b3", "a4", "b1", "d3", "e4", "f5", "g6", "h7",
          "a2", "b2", "d2", "e2", "f2", "g2", "h2", "c1", "c3", "c4", "c5", "c6", "c7", "c8").map(s => Move(queen, s.toSquare)))
    }
  }

  "A Queen in c2 with 2 pawns" should {
    "move to all the direction twoard the chessboard until the pawns" in {
      val queen = board.QueenBoardImpl(White, "c2".toSquare)
      val pawnC6 = board.PawnBoardImpl(White, "c6".toSquare)
      val pawnG2 = board.PawnBoardImpl(Black, "g2".toSquare)
      val pawnG6 = board.PawnBoardImpl(White, "g6".toSquare)
      val pawnB3 = board.PawnBoardImpl(Black, "b3".toSquare)
      val chessboard = ChessboardImpl.empty + queen + pawnC6 + pawnG2 + pawnG6 + pawnB3
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(queen).toSeq.map(_.dest) should containTheSameElementsAs(
          Seq("d1", "b3", "b1", "d3", "e4", "f5",
            "a2", "b2", "d2", "e2", "f2", "g2", "c1", "c3", "c4", "c5").map(s => s.toSquare))
    }
  }

  "A Knight in c2" should {
    "move to a1, a3, b4, d4, e3, e1" in {
      val knight = board.KnightBoardImpl(White, "c2".toSquare)
      val chessboard = ChessboardImpl.empty + knight
      generateMoveWithControl(Tools(chessboard, logBook), White).moves.toSeq should containTheSameElementsAs(
        Seq("a1", "a3", "b4", "d4", "e3", "e1").map(s => Move(knight, s.toSquare)))
    }
  }

  "A Knight in c2 with a pawn" should {
    "move to a1, b4, d4, e3, e1" in {
      val knight = board.KnightBoardImpl(White, "c2".toSquare)
      val pawnA3 = board.PawnBoardImpl(White, "a3".toSquare)
      val chessboard = ChessboardImpl.empty + knight + pawnA3
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(knight).toSeq should containTheSameElementsAs(
          Seq("a1", "b4", "d4", "e3", "e1").map(s => Move(knight, s.toSquare)))
    }
  }

  "A white pawn in c2" should {
    "move to c3, c4" in {
      val pawn = board.PawnBoardImpl(White, "c2".toSquare)
      val chessboard = ChessboardImpl.empty + pawn
      generateMoveWithControl(Tools(chessboard, logBook), White).controls.toSeq should containTheSameElementsAs(
        Seq("b3", "d3").map(s => Move(pawn, s.toSquare)))
      generateMoveWithControl(Tools(chessboard, logBook), White).moves.toSeq should containTheSameElementsAs(
        Seq("c3", "c4").map(s => Move(pawn, s.toSquare)))
    }
  }

  "A white pawn in c2 blocked by a white pawn in c4" should {
    "move to c3" in {
      val pawn = board.PawnBoardImpl(White, "c2".toSquare)
      val pawnC4 = board.PawnBoardImpl(White, "c4".toSquare)
      val chessboard = ChessboardImpl.empty + pawn + pawnC4
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(pawn).toSeq should containTheSameElementsAs(
          Seq("c3").map(s => Move(pawn, s.toSquare)))
    }
  }

  "A white pawn in c2 blocked by a black pawn in c4" should {
    "move to c3" in {
      val pawn = board.PawnBoardImpl(White, "c2".toSquare)
      val pawnC4 = board.PawnBoardImpl(Black, "c4".toSquare)
      val chessboard = ChessboardImpl.empty + pawn + pawnC4
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(pawn).toSeq should containTheSameElementsAs(
          Seq("c3").map(s => Move(pawn, s.toSquare)))
    }
  }

  "A white pawn in c2 blocked by a black pawn in c3" should {
    "not be able to move" in {
      val pawn = board.PawnBoardImpl(White, "c2".toSquare)
      val pawnC3 = board.PawnBoardImpl(Black, "c3".toSquare)
      val chessboard = ChessboardImpl.empty + pawn + pawnC3
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(pawn).toSeq should be empty
    }
  }

  "A white pawn in c2" should {
    "be able to take a black pawn in b3 or d3" in {
      val pawn = board.PawnBoardImpl(White, "c2".toSquare)
      val pawnB3 = board.PawnBoardImpl(Black, "b3".toSquare)
      val pawnC3 = board.PawnBoardImpl(Black, "d3".toSquare)
      val chessboard = ChessboardImpl.empty + pawn + pawnB3 + pawnC3
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(pawn).toSeq.map(_.dest) should containTheSameElementsAs(
          Seq("c3", "c4", "b3", "d3").map(s => s.toSquare))
    }
  }

  "A white pawn in c7" should {
    "promote to a rook, knight, bishop or queen in c8" in {
      val pawn = board.PawnBoardImpl(White, "c7".toSquare)
      val chessboard = ChessboardImpl.empty + pawn
      generateMoveWithControl(Tools(chessboard, logBook), White).moves.toSeq should containTheSameElementsAs(
        Seq(
          Promotion(pawn, board.KnightBoardImpl(White, "c8".toSquare)),
          Promotion(pawn, board.BishopBoardImpl(White, "c8".toSquare)),
          Promotion(pawn, board.RookBoardImpl(White, "c8".toSquare)),
          Promotion(pawn, board.QueenBoardImpl(White, "c8".toSquare))))
    }
  }

  "A white pawn in c7" should {
    "not promote if the case c8 is not empty" in {
      val pawn = board.PawnBoardImpl(Black, "c7".toSquare)
      val pawnC8 = board.PawnBoardImpl(White, "c8".toSquare)
      val chessboard = ChessboardImpl.empty + pawn + pawnC8
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(pawn).toSeq should be empty
    }
  }

  "A black pawn in c2" should {
    "not promote if the case c1 is not empty" in {
      val pawn = board.PawnBoardImpl(Black, "c7".toSquare)
      val pawnC1 = board.RookBoardImpl(White, "c1".toSquare)
      val chessboard = ChessboardImpl.empty + pawnC1
      generateMoveWithControl(Tools(chessboard, logBook), White).moves
        .filterPiece(pawn).toSeq should be empty
    }
  }

}
