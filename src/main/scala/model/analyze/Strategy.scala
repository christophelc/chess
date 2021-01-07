package model.analyze

import model.{ Black, Tools, White }

object Strategy {
  trait GameIdentification
  case object Opening extends GameIdentification
  case object MiddleGame extends GameIdentification
  case object EndGame extends GameIdentification

  def whichGame(tools: Tools): GameIdentification = {
    if (tools.logBook.moves.size < 12 || // approximation
      (tools.chessboard.isCastleAvailable(tools.logBook, White) &&
        tools.chessboard.isCastleAvailable(tools.logBook, Black))) {
      Opening
    } else {
      if (tools.chessboard.pieces.queens.count == 0) {
        EndGame
      } else {
        MiddleGame
      }
    }
  }

  trait Target
  trait TargetOpening extends Target
  case object BishopKnightControl extends TargetOpening
  case object SecurityKingOpening extends TargetOpening
  trait TargetMiddleGame extends Target
  case object RookCoordination extends TargetMiddleGame
  case object SecurityKingMiddleGame extends TargetMiddleGame
  case object SquareControl extends TargetMiddleGame
  trait TargetEndGame extends Target

  def targets(tools: Tools): Seq[Target] = {
    whichGame(tools) match {
      case Opening => Seq(BishopKnightControl, SecurityKingOpening)
      case MiddleGame => Seq(SecurityKingMiddleGame, SquareControl, RookCoordination)
      case EndGame => Nil
    }
  }

}
