package model

import model.Piece.idPawn

case class Tools(chessboard: Chessboard, logBook: LogBook) {

  private def updateEp(move: GenericMove): Tools = {
    val NoEp: Option[Square] = None
    this.copy(logBook = logBook.copy(epForLastMove =
      if (move.piece.id == idPawn &&
        math.abs(move.dest.whichRow - move.piece.position.whichRow) == 2) {
        Seq(
          (move.dest.left, Direction.right),
          (move.dest.right, Direction.left)).foldLeft(NoEp)((maybeEp, squareAndDir) =>
            chessboard
              .get(squareAndDir._1)
              .filter(p => p.id == idPawn && p.color == move.piece.color.invert)
              .map(opponentPawn => Some(opponentPawn.position.shift(squareAndDir._2)))
              .getOrElse(maybeEp))
      } else
        NoEp))
  }

  def play(move: GenericMove): Tools =
    this.copy(
      chessboard = chessboard.play(move),
      logBook = logBook.add(move))
      .updateEp(move)

  def playAndUpdateControls(move: GenericMove): Tools = {
    this.copy(
      chessboard = chessboard.play(move).updateControls(logBook),
      logBook = logBook.add(move))
      .updateEp(move)
  }
}
