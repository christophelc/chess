package model.board

import config.ConfigurationChessboard.ConfigurationCurrentPieceBoardPiece
import config.ConfigurationChessboard.MovesStorage
import model._

case class QueenBoard(
  override val color: Color,
  override val position: Square) extends ConfigurationCurrentPieceBoardPiece with Queen {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook)(emptyMove: MovesStorage): MovesStorage =
    moveMulGenWithControl(chessboard)(emptyMove: MovesStorage)(Seq(
      Direction.left,
      Direction.right,
      Direction.up,
      Direction.down,
      Direction.left.up,
      Direction.left.down,
      Direction.right.up,
      Direction.right.down))
}
