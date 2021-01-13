package model.analyze

import model.Piece.pawnInitialRow
import model.{ Chessboard, _ }
import model.analyze.Strategy._
import model.board.BaseMove.Moves
import model.board._

object Score {

  def value(piece: Piece): Int = {
    piece match {
      case _: Rook => 500
      case _: Bishop => 300
      case _: Knight => 300
      case _: Queen => 1000
      case pawn: Pawn =>
        val n = math.abs(pawn.position.whichRow.minus(pawnInitialRow(pawn.color)))
        100 + (if (n >= 5) n * 20 else 0)
      case _ => 0
    }
  }

  def scoreCapture(chessboard: Chessboard, move: GenericMove): Int = {
    if (chessboard.isAttackedByColor(move.dest, move.piece.color.invert)) {
      move.takenPiece.map(_ => Score.value(move.piece)).getOrElse(0)
    } else
      0
  }
  def value(color: Color, pieces: Pieces): Int = {
    val piecePerColor = pieces.groupByColor
    piecePerColor(color).list.map(value).sum - piecePerColor(color.invert).list.map(value).sum
  }

  // opening
  def developmentBishopKnight(color: Color, controls: Moves): Int = {
    val bishopAndKnightControl = controls.toSeq.filter(_.piece match {
      case _: Knight | _: Bishop => true
      case _ => false
    })
    bishopAndKnightControl.count(_.piece.color == color) * 10
  }

  def developmentRook(color: Color, chessboard: Chessboard): Int = {
    val rooks = chessboard.pieces.rooks
    if (rooks.count == 2 &&
      rooks.list.head.position.squaresStrictlyBetween(rooks.list.last.position).forall(
        square => chessboard.get(square).isEmpty)) {
      5
    } else
      0
  }

  // king security
  def kingSecurity(tools: Tools, color: Color): Int = {
    def kingSecurityPerColor(color: Color): Int = {
      val king = tools.chessboard.findKing(color)
      val castled = if (tools.logBook.kingCastled(king)) 30 else 0
      val verticalDirection = Piece.verticalDirection(color)
      val row = (king.position.whichRow.add(verticalDirection.vertical))
      val col = king.position.whichCol
      // protection by pawns
      Seq(SquareXY(row, col.minus(1)), SquareXY(row, col), SquareXY(row, col.add(1)))
        .count(square => square.isInsideChessboard &&
          (tools.chessboard.get(square) match {
            case Some(_: Pawn) => true
            case _ => false
          })) * 10 + castled
    }
    kingSecurityPerColor(color) - kingSecurityPerColor(color.invert)
  }

  def squareControl(color: Color, tools: Tools): Int = {
    developmentBishopKnight(color, ChessboardImpl.convert(tools.chessboard).controls) +
      developmentRook(color, tools.chessboard)
  }

  def queenActivate(color: Color, controls: Moves): Int = {
    controls.toSeq.count(_.piece match {
      case queen: Queen if (queen.color == color) => true
      case _ => false
    }) * 5
  }

  def rookActivate(color: Color, controls: Moves): Int = {
    controls.toSeq.count(_.piece match {
      case rook: Rook if (rook.color == color) => true
      case _ => false
    }) * 5
  }

  def evaluateOpening(target: Target, color: Color, tools: Tools): Int = {
    target match {
      case BishopKnightControl =>
        squareControl(color, tools) - squareControl(color.invert, tools)
      case SecurityKingOpening => kingSecurity(tools, color)
    }
  }
  def evaluateMiddleGame(target: Target, color: Color, tools: Tools): Int = {
    target match {
      case RookCoordination =>
        developmentRook(color, tools.chessboard) - developmentRook(color.invert, tools.chessboard)
      case SecurityKingMiddleGame => kingSecurity(tools, color)
      case SquareControl =>
        val chessboard = ChessboardImpl.convert(tools.chessboard)
        squareControl(color, tools) - squareControl(color.invert, tools) +
          queenActivate(color, chessboard.controls) - queenActivate(color.invert, chessboard.controls) +
          rookActivate(color, chessboard.controls) - rookActivate(color.invert, chessboard.controls)
    }
  }
  def evaluateEndGame(target: Target, color: Color, tools: Tools): Int = {
    0
  }

  def evaluateTarget(target: Target, color: Color, tools: Tools): Int = {
    target match {
      case targetOpening: TargetOpening => evaluateOpening(targetOpening, color, tools)
      case targetMiddleGame: TargetMiddleGame => evaluateMiddleGame(targetMiddleGame, color, tools)
      case targetEndGame: TargetEndGame => evaluateEndGame(targetEndGame, color, tools)
    }
  }

  def evaluate(color: Color, tools: Tools): Int = {
    val initialScore = value(color, tools.chessboard.pieces)
    Strategy.targets(tools).foldLeft(initialScore)((score, target) =>
      evaluateTarget(target, color, tools) + score)
  }
}
