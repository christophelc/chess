package model.board

import config.ConfigurationChessboard.CurrentMoveStoragePiece
import model.Chessboard.MovesStorage
import model._

trait PieceBoardBase extends Piece with CurrentMoveStoragePiece {

  def checkMoveEmptySquare(chessboard: Chessboard)(move: GenericMove): Option[GenericMove] =
    Option(move).filter(_ => chessboard.get(move.dest).isEmpty)

  def checkMoveInsideChessboard(move: GenericMove): Option[GenericMove] = {
    Option(move).filter(_ => move.dest.isInsideChessboard)
  }

  def addControlAndMoveIfTaking(move: GenericMove, pieceDest: Piece): GenericMove =
    if (pieceDest.color != move.piece.color) {
      move.enableTag(TagIsMove).enableTag(TagIsControl)
    } else {
      move.enableTag(TagIsControl).disableTag(TagIsMove)
    }

  def moveUnitGenWithControl(chessboard: Chessboard)(directions: Seq[Direction]): MovesStorage = {
    def _moveUnitGenWithControl(chessboard: Chessboard)(direction: Direction): MovesStorage = {
      moveUnitWithoutControl(chessboard)(direction) match {
        case None => emptyMove
        case Some(move) =>
          chessboard.get(move.dest) match {
            case None => emptyMove.add(move.piece)(
              Seq(move
                .enableTag(TagIsMove)
                .enableTag(TagIsControl)))
            case Some(pieceDest) =>
              emptyMove.add(move.piece)(
                Seq(addControlAndMoveIfTaking(move, pieceDest)))
          }
      }
    }
    directions.foldLeft(emptyMove)((movesWithControls, direction) =>
      movesWithControls.add(_moveUnitGenWithControl(chessboard)(direction)))
  }

  /**
   * Generate possible moves before verifying if it is invalid due to a check
   *
   * @param chessboard
   * @return a list of possible move
   */
  def moveUnitWithoutControl(chessboard: Chessboard)(direction: Direction): Option[GenericMove] = {
    val dest = shift(direction)
    val move = Move(
      piece = this,
      dest = dest,
      takenPiece = chessboard.get(dest),
      MoveTag.isMove)
    checkMoveInsideChessboard(move)
  }

  def moveMulGenWithControl(chessboard: Chessboard)(directions: Seq[Direction]): MovesStorage = {
    def moveMulWithControl(chessboard: Chessboard)(directionUnit: Direction): MovesStorage = {
      def _moveMul(chessboard: Chessboard)(direction: Direction): MovesStorage = {
        moveUnitWithoutControl(chessboard)(direction) match {
          case None => emptyMove
          case Some(move) =>
            chessboard.get(move.dest) match {
              case None =>
                _moveMul(chessboard)(direction.compose(directionUnit)).add(move.piece)(
                  Seq(move
                    .enableTag(TagIsMove)
                    .enableTag(TagIsControl)))
              case Some(pieceDest) =>
                emptyMove.add(move.piece)(
                  Seq(addControlAndMoveIfTaking(move, pieceDest)))
            }
        }
      }

      _moveMul(chessboard)(directionUnit)
    }

    directions.foldLeft(emptyMove)((movesWithControls, direction) =>
      movesWithControls.add(moveMulWithControl(chessboard)(direction)))
  }
}