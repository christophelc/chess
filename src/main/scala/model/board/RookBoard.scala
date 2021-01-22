package model.board

import config.ConfigurationChessboard.ConfigurationCurrentPieceBoardPiece
import config.ConfigurationChessboard.MovesStorage
import model._

case class RookBoard(
  override val color: Color,
  override val position: Square) extends ConfigurationCurrentPieceBoardPiece with Rook {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage =
    moveMulGenWithControl(chessboard)(Seq(
      Direction.left,
      Direction.right,
      Direction.up,
      Direction.down))
}
