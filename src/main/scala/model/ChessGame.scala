package model

import model.board.ChessboardImplConfiguration$Piece

case class ChessGame(
  playerBlack: Player,
  playerWhite: Player,
  whichPlayerTurn: Color = White,
  val tools: Tools = Tools(chessboard = ChessboardImplConfiguration$Piece(), logBook = LogBook()),
  timer: Timer) {

  def name: String = s"${playerWhite.name}-${playerBlack.name}"

  def playerColor(color: Color): Player =
    if (color == Black)
      playerBlack
    else
      playerWhite

  def play(whichPlayer: Color): ChessGame = {
    this.copy(
      tools = playerColor(whichPlayer).play(tools, whichPlayer))
  }
}

