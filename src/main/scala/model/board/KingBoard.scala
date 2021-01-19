package model.board

import RichSquare.SquareXYFromString
import model.Chessboard.MovesStorage
import model.{ Black, Chessboard, Color, Direction, GenericMove, King, LogBook, Piece, Square, White }

object KingBoard {
  final val e1 = "e1".toSquare
  final val e8 = "e8".toSquare
  def initialPosition(color: Color): Square = color match {
    case White => e1
    case Black => e8
  }
}

case class KingBoard(
  override val color: Color,
  override val position: Square) extends PieceBoard(color, position) with King {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage = {
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

    val movesAllDirection: MovesStorage =
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
    movesAllDirection.add(this)(movesCastling
      .map(_.disableTag(TagIsControl).enableTag(TagIsMove)))
  }
}