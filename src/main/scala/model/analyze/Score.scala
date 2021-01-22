package model.analyze

import config.ConfigurationChessboard.MovesStorage
import model.Piece.{ PieceId, idBishop, idKnight, idQueen, idRook, pawnInitialRow }
import model.{ Chessboard, _ }
import model.analyze.Strategy._
import model.board._

trait Selector[K] {
  def selector(controls: MovesStorage)(id: PieceId, color: Color): MovesStorage
  def selectorMultiple(controls: MovesStorage)(id: Seq[PieceId], color: Color): MovesStorage
  def selectorWithFilter(controls: MovesStorage)(filter: (PieceId, Color) => Boolean): MovesStorage
}

abstract class ScoreGen[K] extends Selector[K] {

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
      move.takenPiece.map(_ => value(move.piece)).getOrElse(0)
    } else
      0
  }
  def valuePiece(color: Color, pieces: Pieces): Int =
    pieces.toSeq.map(piece => if (piece.color == color) value(piece) else -value(piece)).sum

  // opening
  def developmentBishopKnight(color: Color, controls: MovesStorage): Int =
    selectorMultiple(controls)(Seq(idKnight, idBishop), color).countV * 10

  def developmentRook(color: Color, chessboard: Chessboard): Int = {
    val rooks = chessboard.pieces.rooks
    if (rooks.count == 2 &&
      rooks.toSeq.head.position.squaresStrictlyBetween(rooks.toSeq.last.position).forall(
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
    developmentBishopKnight(color, tools.chessboard.moves.filterV(_.isTagged(TagIsControl))) +
      developmentRook(color, tools.chessboard)
  }

  def queenActivate(color: Color, controls: MovesStorage): Int =
    selector(controls)(idQueen, color).countV * 5

  def rookActivate(color: Color, controls: MovesStorage): Int =
    selector(controls)(idRook, color).countV * 5

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
        val controls = tools.chessboard.moves.filterV(_.isTagged(TagIsControl))
        squareControl(color, tools) - squareControl(color.invert, tools) +
          queenActivate(color, controls) - queenActivate(color.invert, controls) +
          rookActivate(color, controls) - rookActivate(color.invert, controls)
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
    val initialScore = valuePiece(color, tools.chessboard.pieces)
    Strategy.targets(tools).foldLeft(initialScore)((score, target) =>
      evaluateTarget(target, color, tools) + score)
  }
}

trait SelectorPiece extends Selector[Piece] {
  override def selector(controls: MovesStorage)(filterPieceId: PieceId, filterColor: Color): MovesStorage =
    selectorWithFilter(controls)((pieceId: PieceId, color: Color) => filterPieceId == pieceId && filterColor == color)

  override def selectorMultiple(controls: MovesStorage)(filterPieceId: Seq[PieceId], filterColor: Color): MovesStorage =
    selectorWithFilter(controls)((pieceId: PieceId, color: Color) => filterPieceId.contains(pieceId) && filterColor == color)

  override def selectorWithFilter(controls: MovesStorage)(filter: (PieceId, Color) => Boolean): MovesStorage = {
    controls.filterK {
      case k: Piece if filter(k.id, k.color) => true
      case _ => false
    }
  }
}

object Score extends ScoreGen[Piece] with SelectorPiece