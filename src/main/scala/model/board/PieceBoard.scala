package model.board

import model.Piece._
import RichSquare.SquareXYFromString
import model._

abstract class PieceBoard(
  override val color: Color,
  override val position: Square) extends Piece {

  def checkMoveEmptySquare(chessboard: Chessboard)(move: GenericMove): Option[GenericMove] =
    Option(move).filter(_ => chessboard.get(move.dest).isEmpty)

  def checkMoveInsideChessboard(chessboard: Chessboard)(move: GenericMove): Option[GenericMove] = {
    Option(move).filter(_ => move.dest.isInsideChessboard)
  }

  def moveUnitGenWithControl(chessboard: Chessboard)(directions: Seq[Direction]): MovesWithControl = {
    def _moveUnitGenWithControl(chessboard: Chessboard)(direction: Direction): MovesWithControl = {
      val emptyMove = MovesWithControlImpl()
      moveUnitWithoutControl(chessboard)(direction) match {
        case None => emptyMove
        case Some(move) =>
          chessboard.get(move.dest) match {
            case None => emptyMove.addMoveAndControl(move)
            case Some(pieceDest) => emptyMove.addMoveOrControlOnlyIfSameColor(move, pieceDest)
          }
      }
    }
    directions.foldLeft(MovesWithControlImpl())((movesWithControls, direction) =>
      MovesWithControlImpl.convert(movesWithControls.concat(_moveUnitGenWithControl(chessboard)(direction))))
  }

  /**
   * Generate possible moves before verifying if it is invalid due to a check
   *
   * @param chessboard
   * @return a list of possible move
   */
  def moveUnitWithoutControl(chessboard: Chessboard)(direction: Direction): Option[GenericMove] = {
    val dest = shift(direction)
    val move = Move(
      piece = this,
      dest = dest,
      takenPiece = chessboard.get(dest))
    checkMoveInsideChessboard(chessboard)(move)
  }

  def moveMulGenWithControl(chessboard: Chessboard)(directions: Seq[Direction]): MovesWithControl = {
    def moveMulWithControl(chessboard: Chessboard)(directionUnit: Direction): MovesWithControl = {
      def _moveMul(chessboard: Chessboard)(direction: Direction): MovesWithControl = {
        moveUnitWithoutControl(chessboard)(direction) match {
          case None => MovesWithControlImpl()
          case Some(move) =>
            chessboard.get(move.dest) match {
              case None => _moveMul(chessboard)(direction.compose(directionUnit)).addMoveAndControl(move)
              case Some(pieceDest) => MovesWithControlImpl().addMoveOrControlOnlyIfSameColor(move, pieceDest)
            }
        }
      }

      _moveMul(chessboard)(directionUnit)
    }

    directions.foldLeft(MovesWithControlImpl())((movesWithControls, direction) =>
      MovesWithControlImpl.convert(movesWithControls.concat(moveMulWithControl(chessboard)(direction))))
  }
}

case class QueenBoardImpl(
  override val color: Color,
  override val position: Square) extends PieceBoard(color, position) with Queen {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesWithControl =
    moveMulGenWithControl(chessboard)(Seq(
      Direction.left,
      Direction.right,
      Direction.up,
      Direction.down,
      Direction.left.up,
      Direction.left.down,
      Direction.right.up,
      Direction.right.down))
}

object KingBoardImpl {
  final val e1 = "e1".toSquare
  final val e8 = "e8".toSquare
  def initialPosition(color: Color): Square = color match {
    case White => e1
    case Black => e8
  }
}

case class KingBoardImpl(
  override val color: Color,
  override val position: Square) extends PieceBoard(color, position) with King {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesWithControl = {
    val smallCastling: Option[GenericMove] = if (chessboard.canSmallCastling(logBook, king = this))
      moveUnitWithoutControl(chessboard)(Direction.right).flatMap(_ =>
        chessboard.get(this.position.right.right.right).flatMap(rook =>
          checkMoveEmptySquare(chessboard)(SmallCastling(
            king = this,
            dest = this.position.right.right,
            rookMove = Move(
              piece = rook,
              dest = this.position.right)))))
    else
      None
    val greatCastling = if (chessboard.canGreatCastling(logBook, king = this))
      moveUnitWithoutControl(chessboard)(Direction.left).flatMap(_ =>
        chessboard.get(this.position.left.left.left.left).flatMap(rook =>
          checkMoveEmptySquare(chessboard)(SmallCastling(
            king = this,
            dest = this.position.left.left,
            rookMove = Move(
              piece = rook,
              dest = this.position.left)))))
    else
      None

    val movesAllDirection: MovesWithControl =
      moveUnitGenWithControl(chessboard)(Seq(
        Direction.left,
        Direction.right,
        Direction.up,
        Direction.down,
        Direction.left.up,
        Direction.left.down,
        Direction.right.up,
        Direction.right.down))
    val movesCastling: Seq[GenericMove] = smallCastling.toSeq ++ greatCastling.toSeq
    movesAllDirection.addMovesOnly(piece = this, moves = movesCastling)
  }
}

case class KnightBoardImpl(
  override val color: Color,
  override val position: Square) extends PieceBoard(color, position) with Knight {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesWithControl =
    moveUnitGenWithControl(chessboard)(Seq(
      Direction.left.left.up,
      Direction.left.left.down,
      Direction.right.right.up,
      Direction.right.right.down,
      Direction.up.up.left,
      Direction.up.up.right,
      Direction.down.down.left,
      Direction.down.down.right))
}
case class BishopBoardImpl(
  override val color: Color,
  override val position: Square) extends PieceBoard(color, position) with Bishop {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesWithControl =
    moveMulGenWithControl(chessboard)(Seq(
      Direction.left.up,
      Direction.left.down,
      Direction.right.up,
      Direction.right.down))
}

case class RookBoardImpl(
  override val color: Color,
  override val position: Square) extends PieceBoard(color, position) with Rook {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesWithControl =
    moveMulGenWithControl(chessboard)(Seq(
      Direction.left,
      Direction.right,
      Direction.up,
      Direction.down))
}
case class PawnBoardImpl(
  override val color: Color,
  override val position: Square) extends PieceBoard(color, position) with Pawn {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def display: String = if (color == Black) "♙" else "♟"
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesWithControl = {
    def promotion(move: GenericMove): Seq[Promotion] = Seq(
      Promotion(this, QueenBoardImpl(color = this.color, position = move.dest), takenPiece = chessboard.get(move.dest)),
      Promotion(this, KnightBoardImpl(color = this.color, position = move.dest), takenPiece = chessboard.get(move.dest)),
      Promotion(this, BishopBoardImpl(color = this.color, position = move.dest), takenPiece = chessboard.get(move.dest)),
      Promotion(this, RookBoardImpl(color = this.color, position = move.dest), takenPiece = chessboard.get(move.dest)))

    val (verticalDirection, pawnInitialRow) = (Piece.verticalDirection(color), Piece.pawnInitialRow(color))
    val diagonalLeft = verticalDirection.left
    val diagonalRight = verticalDirection.right
    val verticalMove1 = moveUnitWithoutControl(chessboard)(verticalDirection)
      .filter(move => chessboard.get(move.dest).isEmpty)
    val verticalMove2 = verticalMove1
      .filter(_ => position.whichRow == pawnInitialRow)
      .flatMap(_ => moveUnitWithoutControl(chessboard)(verticalDirection.compose(verticalDirection))
        .filter(move => chessboard.get(move.dest).isEmpty))
    val diagLeft = moveUnitWithoutControl(chessboard)(diagonalLeft)
      .filter(move => chessboard.get(move.dest).exists(_.color == color.invert))
    val diagRight = moveUnitWithoutControl(chessboard)(diagonalRight)
      .filter(move => chessboard.get(move.dest).exists(_.color == color.invert))
    val ep = logBook.moves.lastOption.filter {
      move: GenericMove =>
        move.piece match {
          case pawn: Pawn if math.abs(pawn.position.whichRow.minus(move.dest.whichRow)) == 2 &&
            move.dest.whichRow == this.position.whichRow &&
            math.abs(this.position.whichCol.minus(pawn.position.whichCol)) == 1 => true
          case _ => false
        }
    }.map(lastMovePawn2squares =>
      Ep(
        pawn = this,
        dest = position.shift(Direction(
          vertical = verticalDirection.vertical,
          horizontal = lastMovePawn2squares.piece.position.whichCol.minus(this.position.whichCol))),
        takenPawn = PawnBoardImpl(lastMovePawn2squares.piece.color, lastMovePawn2squares.dest)))
    val movesOnly = Seq(verticalMove1, verticalMove2, diagLeft, diagRight, ep)
      .flatten
      .flatMap(move => if (move.dest.isRowLimit)
        promotion(move)
      else
        Seq(move))

    MovesWithControlImpl
      .build(
        piece = this,
        moves = movesOnly,
        controls = Seq(
          moveUnitWithoutControl(chessboard)(diagonalLeft),
          moveUnitWithoutControl(chessboard)(diagonalRight)).flatten)
  }
}