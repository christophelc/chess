package model

import model.Chessboard.MovesStorage
import org.specs2.mutable.Specification
import model.board.RichSquare._
import model.board.StorageImpl.emptyMoveStorage
import model.board._

class ChessboardSpec extends Specification {

  val logBook: LogBook = LogBook()

  def generateMoveWithControl(tools: Tools, color: Color): MovesStorage =
    tools.chessboard.generateMoveWithControl(color)(tools.logBook)
      .filterV(_.isTagged(TagIsMove))

  "A King doing small castle" should {
    "be at g1 and its rook at f1" in {
      val king = KingBoardImpl(White, "e1".toSquare)
      val rook = RookBoardImpl(White, "h1".toSquare)
      val chessboard = ChessboardImpl.empty + king + rook
      chessboard.isCastleAvailableNow(logBook, White) should beTrue
      val move = generateMoveWithControl(Tools(chessboard, logBook), White)
        .findV(_.dest == "g1".toSquare)
      move.isDefined should beTrue
      val newChessboard = chessboard.play(move.get)
      val newLogBook = logBook.add(move.get)
      val kingMoved = board.KingBoardImpl(White, "g1".toSquare)
      newChessboard shouldEqual ChessboardImpl.empty + kingMoved + board.RookBoardImpl(White, "f1".toSquare)
      newChessboard.isCastleAvailableNow(newLogBook, White) should beFalse
    }
  }

  "A King that has moved" should {
    "not be able to castle" in {
      val king = board.KingBoardImpl(White, "e1".toSquare)
      val rook = board.RookBoardImpl(White, "h1".toSquare)
      val chessboard = ChessboardImpl.empty + king + rook
      chessboard.isCastleAvailableNow(logBook, White) should beTrue
      val move = generateMoveWithControl(Tools(chessboard, logBook), White)
        .findV(_.dest == "f1".toSquare)
      move.isDefined should beTrue
      val newChessboard = chessboard.play(move.get)
      val newLogBook = logBook.add(move.get)
      val kingMoved = board.KingBoardImpl(White, "f1".toSquare)
      newChessboard.isCastleAvailableNow(newLogBook, White) should beFalse
    }
  }

  "A promoting white pawn into a Rook in c8" should {
    "promote to a rook in c8" in {
      val pawn = PawnBoardImpl(White, "c7".toSquare)
      val chessboard = ChessboardImpl.empty + pawn
      val promoted = board.RookBoardImpl(White, "c8".toSquare)
      val move = generateMoveWithControl(Tools(chessboard, logBook), White)
        .findV {
          case Promotion(_, newPiece, _, _) => newPiece match {
            case _: Rook => true
            case _ => false
          }
        }
      move.isDefined should beTrue
      chessboard.play(move.get) shouldEqual ChessboardImpl.empty + promoted
    }
  }

  "A promoting white pawn into a Knight in c8" should {
    "promote to a Knight in c8" in {
      val pawn = board.PawnBoardImpl(White, "c7".toSquare)
      val chessboard = ChessboardImpl.empty + pawn
      val promoted = KnightBoardImpl(White, "c8".toSquare)
      val move = generateMoveWithControl(Tools(chessboard, logBook), White)
        .findV {
          case Promotion(_, newPiece, _, _) => newPiece match {
            case _: Knight => true
            case _ => false
          }
        }
      move.isDefined should beTrue
      chessboard.play(move.get) shouldEqual ChessboardImpl.empty + promoted
    }
  }

  "A promoting white pawn into a Bishop in c8" should {
    "promote to a Bishop in c8" in {
      val pawn = board.PawnBoardImpl(White, "c7".toSquare)
      val chessboard = ChessboardImpl.empty + pawn
      val promoted = BishopBoardImpl(White, "c8".toSquare)
      val move = generateMoveWithControl(Tools(chessboard, logBook), White)
        .findV {
          case Promotion(_, newPiece, _, _) => newPiece match {
            case _: Bishop => true
            case _ => false
          }
        }
      move.isDefined should beTrue
      chessboard.play(move.get) shouldEqual ChessboardImpl.empty + promoted
    }
  }

  "A promoting white pawn into a Queen in c8" should {
    "promote to a Queen in c8" in {
      val whiteKing = board.KingBoardImpl(White, "a6".toSquare)
      val blackKing = board.KingBoardImpl(Black, "a1".toSquare)
      val pawn = board.PawnBoardImpl(White, "c7".toSquare)
      val tools = Tools(
        chessboard = ChessboardImpl.empty + pawn + whiteKing + blackKing,
        logBook = logBook)
      val promoted = QueenBoardImpl(White, "c8".toSquare)
      val moves: MovesStorage = generateMoveWithControl(tools, White)
      val move = moves
        .findV {
          case Promotion(_, newPiece, _, _) => newPiece match {
            case _: Queen => true
            case _ => false
          }
        }
      move.isDefined should beTrue
      tools.chessboard.play(move.get) shouldEqual ChessboardImpl.empty + promoted + whiteKing + blackKing
      move.get.show(tools, emptyMoveStorage.add(moves)) shouldEqual ("c8=Q")
    }

    "promote to a Queen in cxb8 by taking a black piece in b8" in {
      val whiteKing = board.KingBoardImpl(White, "a6".toSquare)
      val blackKing = board.KingBoardImpl(Black, "a1".toSquare)
      val pawn = board.PawnBoardImpl(White, "c7".toSquare)
      val rook = board.RookBoardImpl(Black, "b8".toSquare)
      val tools = Tools(
        chessboard = ChessboardImpl.empty + pawn + rook + whiteKing + blackKing,
        logBook = logBook)
      val promoted = board.QueenBoardImpl(White, "b8".toSquare)
      val moves = generateMoveWithControl(tools, White)
        .filterV(_.isTagged(TagIsMove))
      val move = moves
        .filterV(_.dest == "b8".toSquare)
        .findV {
          case Promotion(_, newPiece, _, _) => newPiece match {
            case _: Queen => true
            case _ => false
          }
          case _ => false
        }
      move.isDefined should beTrue
      tools.chessboard.play(move.get) shouldEqual ChessboardImpl.empty + promoted + whiteKing + blackKing
      move.get.show(tools, moves) shouldEqual ("cxb8=Q")
    }
  }

  "A e.p when white move c4 and black pawn in b4" should {
    "be able to make the move cxb3" in {
      val pawnEp = board.PawnBoardImpl(White, "c2".toSquare)
      val takingPawn = board.PawnBoardImpl(Black, "b4".toSquare)
      val expectedEp = board.PawnBoardImpl(Black, "c3".toSquare)
      val king = board.KingBoardImpl(White, "a8".toSquare)
      val tools = Tools(
        chessboard = ChessboardImpl.empty + pawnEp + takingPawn + king,
        logBook = logBook)
      val move: Option[GenericMove] = generateMoveWithControl(tools, White)
        .findV(_.dest == "c4".toSquare)
      move.isDefined should beTrue
      val newChessboard = tools.chessboard.play(move.get)
      val newTools = tools.copy(logBook = tools.logBook.add(move.get))
      val moves = generateMoveWithControl(Tools(newChessboard, newTools.logBook), Black)
      val moveEp: Option[GenericMove] = moves
        .findV(_.dest == "c3".toSquare)
      moveEp.isDefined should beTrue
      moveEp.get.show(newTools, moves) shouldEqual ("bxc3")
      newChessboard.play(moveEp.get) shouldEqual ChessboardImpl.empty + expectedEp + king
    }

  }
}
