package model.board

import model.Chessboard.MovesStorage
import model.{ Bishop, Chessboard, Color, Direction, Knight, LogBook, Piece, Square }

case class KnightBoard(
  override val color: Color,
  override val position: Square) extends PieceBoard(color, position) with Knight {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage =
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
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage =
    moveMulGenWithControl(chessboard)(Seq(
      Direction.left.up,
      Direction.left.down,
      Direction.right.up,
      Direction.right.down))
}
