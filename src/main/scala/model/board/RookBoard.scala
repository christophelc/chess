package model.board

import model.Chessboard.MovesStorage
import model.{ Chessboard, Color, Direction, LogBook, Piece, Rook, Square }

case class RookBoard(
  override val color: Color,
  override val position: Square) extends PieceBoard(color, position) with Rook {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage =
    moveMulGenWithControl(chessboard)(Seq(
      Direction.left,
      Direction.right,
      Direction.up,
      Direction.down))
}
