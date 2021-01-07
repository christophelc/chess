package model.board

import model.board.BaseMove.{ EmptyMove, Moves }
import model.{ GenericMove, MovesWithControl, Piece }

object MovesWithControlImpl {

  def convert(movesWithControl: MovesWithControl): MovesWithControlImpl =
    movesWithControl match { case m: MovesWithControlImpl => m }
}

case class MovesWithControlImpl(
  moves: Moves = EmptyMove,
  controls: Moves = EmptyMove) extends MovesWithControl {

  override type Moves = Seq[GenericMove]

  def addMovesOnly(moves: Moves): MovesWithControl = this.copy(moves = this.moves ++ moves)
  def addControlOnly(moves: Moves): MovesWithControl = this.copy(controls = this.controls ++ moves)
  def addMoveAndControl(move: GenericMove): MovesWithControl = this.copy(moves = move +: moves, controls = move +: controls)
  def addMoveOrControlOnlyIfSameColor(move: GenericMove, pieceDest: Piece): MovesWithControl = {
    if (pieceDest.color != move.piece.color)
      this.copy(moves = move +: moves, controls = move +: controls)
    else
      this.copy(controls = move +: controls)
  }

  override def concat(movesWithControl: MovesWithControl): MovesWithControl = {
    this.copy(
      moves = this.moves ++ MovesWithControlImpl.convert(movesWithControl).moves,
      controls = this.controls ++ MovesWithControlImpl.convert(movesWithControl).controls)
  }

  def isEmpty = moves.isEmpty
}