package model.board

import config.ConfigurationChessboard.ConfigurationCurrentPieceBoardPiece
import model.Chessboard.MovesStorage
import model._

case class KnightBoard(
  override val color: Color,
  override val position: Square) extends ConfigurationCurrentPieceBoardPiece with Knight {

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