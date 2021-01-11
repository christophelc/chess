package model.board

import model.board.BaseMove.{ EmptyMove, Moves }
import model.{ GenericMove, MovesWithControl, Piece }

object MovesWithControlImpl {

  def convert(movesWithControl: MovesWithControl): MovesWithControlImpl =
    movesWithControl match { case m: MovesWithControlImpl => m }

  def build(
    piece: Piece,
    moves: Seq[GenericMove] = Nil,
    controls: Seq[GenericMove] = Nil): MovesWithControl =
    MovesWithControlImpl(
      moves = MovesManager(moves = Map(piece -> moves)),
      controls = MovesManager(moves = Map(piece -> controls)))
}

case class MovesWithControlImpl(
  moves: Moves = EmptyMove,
  controls: Moves = EmptyMove) extends MovesWithControl {

  override type Moves = MovesManager

  override def addMovesOnly(piece: Piece, newMoves: Seq[GenericMove]): MovesWithControl =
    this.copy(moves = this.moves.add(
      MovesManager(moves = Map(piece -> newMoves))))
  override def addMoveAndControl(move: GenericMove): MovesWithControl =
    this.copy(moves = moves.add(move), controls = controls.add(move))
  override def addMoveOrControlOnlyIfSameColor(move: GenericMove, pieceDest: Piece): MovesWithControl = {
    if (pieceDest.color != move.piece.color)
      this.copy(moves = moves.add(move), controls = controls.add(move))
    else
      this.copy(controls = controls.add(move))
  }

  override def concat(movesWithControl: MovesWithControl): MovesWithControl = {
    this.copy(
      moves = this.moves.add(MovesWithControlImpl.convert(movesWithControl).moves),
      controls = this.controls.add(MovesWithControlImpl.convert(movesWithControl).controls))
  }

  def isEmpty = moves.isEmpty
}