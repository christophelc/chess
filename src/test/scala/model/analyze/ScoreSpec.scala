package model.analyze

import model.Chessboard.MovesStorage
import model._
import model.Piece._
import org.specs2.mutable.Specification
import model.board.RichSquare._
import model.board.StorageImpl.emptyMoveStorage
import model.board._

class ScoreSpec extends Specification {

  def generateMoveWithControl(tools: Tools, color: Color): MovesStorage =
    tools.chessboard.generateMoveWithControl(color)(tools.logBook)

  "MAterial position" should {
    "be scored" in {
      val logBook = LogBook()
      val kingWhite = KingBoardImpl(White, "e1".toSquare)
      val rookWhite = RookBoardImpl(White, "h1".toSquare)
      val bishopBlack = BishopBoardImpl(Black, "a8".toSquare)
      val kingBlack = KingBoardImpl(Black, "e8".toSquare)
      val chessboard = ChessboardImpl.empty + kingWhite + kingBlack + rookWhite + bishopBlack
      val tools = Tools(chessboard, logBook)
      val score = Score.evaluate(White, tools)
      score shouldEqual 200
    }
  }

  "Scoring" should {
    "make the difference between Black and White position after first move" in {
      val logBook = LogBook()
      val chessboard = ChessboardImpl()
      val tools = Tools(chessboard, logBook)
      val moveNC3 = generateMoveWithControl(Tools(chessboard, logBook), White)
        .filterP(p => p.id == idKnight && p.position.whichCol == Square.colB)
        .findM(_.dest.whichCol == Square.colC)
      moveNC3.isDefined should beTrue
      val move = moveNC3.get
      val tools2 = tools.playAndUpdateControls(move)
      val moves = tools2.chessboard.moves
      val controls = moves.filterM(_.isTagged(TagIsControl))
      controls.countM shouldEqual 82
      val score = Score.evaluate(Black, tools2)
      Score.developmentBishopKnight(Black, controls) shouldEqual 100
      Score.developmentBishopKnight(White, controls) shouldEqual 150
      Score.squareControl(Black, tools2) shouldEqual 100
      Score.squareControl(White, tools2) shouldEqual 150
      score shouldEqual -50
    }
  }

}