package model

import org.specs2.mutable.Specification
import model.board.RichSquare._
import model.board._

class ChessboardSpec extends Specification {

  val logBook: LogBook = LogBook()

  def generateMoveWithControl(tools: Tools, color: Color): Seq[GenericMove] =
    MovesWithControlImpl.convert(tools.chessboard.generateMoveWithControl(color)(tools.logBook)).moves

  "A King doing small castle" should {
    "be at g1 and its rook at f1" in {
      val king = King(White, "e1".toSquare)
      val rook = Rook(White, "h1".toSquare)
      val chessboard = ChessboardImpl.empty + king + rook
      chessboard.isCastleAvailableNow(logBook, White) should beTrue
      val move = generateMoveWithControl(Tools(chessboard, logBook), White)
        .find(_.dest == "g1".toSquare)
      move.isDefined should beTrue
      val newChessboard = chessboard.play(move.get)
      val newLogBook = logBook.add(move.get)
      val kingMoved = board.King(White, "g1".toSquare)
      newChessboard shouldEqual ChessboardImpl.empty + kingMoved + board.Rook(White, "f1".toSquare)
      newChessboard.isCastleAvailableNow(newLogBook, White) should beFalse
    }
  }

  "A King that has moved" should {
    "not be able to castle" in {
      val king = board.King(White, "e1".toSquare)
      val rook = board.Rook(White, "h1".toSquare)
      val chessboard = ChessboardImpl.empty + king + rook
      chessboard.isCastleAvailableNow(logBook, White) should beTrue
      val move = generateMoveWithControl(Tools(chessboard, logBook), White)
        .find(_.dest == "f1".toSquare)
      move.isDefined should beTrue
      val newChessboard = chessboard.play(move.get)
      val newLogBook = logBook.add(move.get)
      val kingMoved = board.King(White, "f1".toSquare)
      newChessboard.isCastleAvailableNow(newLogBook, White) should beFalse
    }
  }

  "A promoting white pawn into a Rook in c8" should {
    "promote to a rook in c8" in {
      val pawn = Pawn(White, "c7".toSquare)
      val chessboard = ChessboardImpl.empty + pawn
      val promoted = board.Rook(White, "c8".toSquare)
      val move = generateMoveWithControl(Tools(chessboard, logBook), White)
        .find(_ match {
          case Promotion(_, newPiece, _) => newPiece match {
            case Rook(_, _) => true
            case _ => false
          }
        })
      move.isDefined should beTrue
      chessboard.play(move.get) shouldEqual ChessboardImpl.empty + promoted
    }
  }

  "A promoting white pawn into a Knight in c8" should {
    "promote to a Knight in c8" in {
      val pawn = board.Pawn(White, "c7".toSquare)
      val chessboard = ChessboardImpl.empty + pawn
      val promoted = Knight(White, "c8".toSquare)
      val move = generateMoveWithControl(Tools(chessboard, logBook), White)
        .find(_ match {
          case Promotion(_, newPiece, _) => newPiece match {
            case Knight(_, _) => true
            case _ => false
          }
        })
      move.isDefined should beTrue
      chessboard.play(move.get) shouldEqual ChessboardImpl.empty + promoted
    }
  }

  "A promoting white pawn into a Bishop in c8" should {
    "promote to a Bishop in c8" in {
      val pawn = board.Pawn(White, "c7".toSquare)
      val chessboard = ChessboardImpl.empty + pawn
      val promoted = Bishop(White, "c8".toSquare)
      val move = generateMoveWithControl(Tools(chessboard, logBook), White)
        .find(_ match {
          case Promotion(_, newPiece, _) => newPiece match {
            case Bishop(_, _) => true
            case _ => false
          }
        })
      move.isDefined should beTrue
      chessboard.play(move.get) shouldEqual ChessboardImpl.empty + promoted
    }
  }

  "A promoting white pawn into a Queen in c8" should {
    "promote to a Queen in c8" in {
      val whiteKing = board.King(White, "a6".toSquare)
      val blackKing = board.King(Black, "a1".toSquare)
      val pawn = board.Pawn(White, "c7".toSquare)
      val tools = Tools(
        chessboard = ChessboardImpl.empty + pawn + whiteKing + blackKing,
        logBook = logBook)
      val promoted = Queen(White, "c8".toSquare)
      val moves = generateMoveWithControl(tools, White)
      val move = moves
        .find(_ match {
          case Promotion(_, newPiece, _) => newPiece match {
            case Queen(_, _) => true
            case _ => false
          }
        })
      move.isDefined should beTrue
      tools.chessboard.play(move.get) shouldEqual ChessboardImpl.empty + promoted + whiteKing + blackKing
      move.get.show(tools, moves) shouldEqual ("c8=Q")
    }

    "promote to a Queen in cxb8 by taking a black piece in b8" in {
      val whiteKing = board.King(White, "a6".toSquare)
      val blackKing = board.King(Black, "a1".toSquare)
      val pawn = board.Pawn(White, "c7".toSquare)
      val rook = board.Rook(Black, "b8".toSquare)
      val tools = Tools(
        chessboard = ChessboardImpl.empty + pawn + rook + whiteKing + blackKing,
        logBook = logBook)
      val promoted = board.Queen(White, "b8".toSquare)
      val moves = generateMoveWithControl(tools, White)
      val move = moves
        .filter(_.dest == "b8".toSquare)
        .find(_ match {
          case Promotion(_, newPiece, _) => newPiece match {
            case Queen(_, _) => true
            case _ => false
          }
        })
      move.isDefined should beTrue
      tools.chessboard.play(move.get) shouldEqual ChessboardImpl.empty + promoted + whiteKing + blackKing
      move.get.show(tools, moves) shouldEqual ("cxb8=Q")
    }
  }

  "A e.p when white move c4 and black pawn in b4" should {
    "be able to make the move cxb3" in {
      val pawnEp = board.Pawn(White, "c2".toSquare)
      val takingPawn = board.Pawn(Black, "b4".toSquare)
      val expectedEp = board.Pawn(Black, "c3".toSquare)
      val king = board.King(White, "a8".toSquare)
      val tools = Tools(
        chessboard = ChessboardImpl.empty + pawnEp + takingPawn + king,
        logBook = logBook)
      val move: Option[GenericMove] = generateMoveWithControl(tools, White)
        .find(_.dest == "c4".toSquare)
      move.isDefined should beTrue
      val newChessboard = tools.chessboard.play(move.get)
      val newTools = tools.copy(logBook = tools.logBook.add(move.get))
      val moves = generateMoveWithControl(Tools(newChessboard, newTools.logBook), Black)
      val moveEp: Option[GenericMove] = moves
        .find(_.dest == "c3".toSquare)
      moveEp.isDefined should beTrue
      moveEp.get.show(newTools, moves) shouldEqual ("bxc3")
      newChessboard.play(moveEp.get) shouldEqual ChessboardImpl.empty + expectedEp + king
    }

  }

}
