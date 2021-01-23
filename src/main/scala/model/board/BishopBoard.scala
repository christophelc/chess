package model.board

import config.ConfigurationChessboard.ConfigurationCurrentPieceBoardPiece
import config.ConfigurationChessboard.MovesStorage
import model._

case class BishopBoard(
  override val color: Color,
  override val position: Square) extends ConfigurationCurrentPieceBoardPiece with Bishop {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook)(emptyMove: MovesStorage): MovesStorage =
    moveMulGenWithControl(chessboard)(emptyMove)(Seq(
      Direction.left.up,
      Direction.left.down,
      Direction.right.up,
      Direction.right.down))
}
