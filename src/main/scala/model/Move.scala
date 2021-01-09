package model

import model.board.BaseMove.Moves

case class Direction(val horizontal: Byte = 0, val vertical: Byte = 0) {
  require(horizontal != 0 || vertical != 0)
  def compose(direction: Direction) = Direction(
    (this.horizontal + direction.horizontal).toByte,
    (this.vertical + direction.vertical).toByte)

  def left: Direction = compose(Direction.left)
  def right: Direction = compose(Direction.right)
  def up: Direction = compose(Direction.up)
  def down: Direction = compose(Direction.down)
}
object Direction {
  def left: Direction = Direction(horizontal = -1)
  def right: Direction = Direction(horizontal = 1)
  def up: Direction = Direction(vertical = 1)
  def down: Direction = Direction(vertical = -1)
}

trait GenericMove {
  val piece: Piece
  val dest: Square
  val takenPiece: Option[Piece] = None

  def showEasy(): String

  /**
   * Take into account check, taking
   * @param tools
   * @param moves
   * @return
   */
  def show(tools: Tools, moves: Moves): String
}

trait MovesWithControl {
  type Moves
  val moves: Moves
  val controls: Moves

  def addMovesOnly(moves: Moves): MovesWithControl
  def addControlOnly(moves: Moves): MovesWithControl
  def addMoveAndControl(move: GenericMove): MovesWithControl
  def addMoveOrControlOnlyIfSameColor(move: GenericMove, pieceDest: Piece): MovesWithControl
  def concat(movesWithControl: MovesWithControl): MovesWithControl
}