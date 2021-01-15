package model.board

import model.Chessboard.MovesStorage
import model.{ Chessboard, Color, Direction, LogBook, Piece, Queen, Square }

case class QueenBoardImpl(
  override val color: Color,
  override val position: Square) extends PieceBoard(color, position) with Queen {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage =
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
