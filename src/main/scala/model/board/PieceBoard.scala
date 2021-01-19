package model.board

import model.Chessboard.MovesStorage
import model._
import model.data.StorageMap.EmptyMoveStorage

abstract class PieceBoard(
  override val color: Color,
  override val position: Square) extends Piece {

  def checkMoveEmptySquare(chessboard: Chessboard)(move: GenericMove): Option[GenericMove] =
    Option(move).filter(_ => chessboard.get(move.dest).isEmpty)

  def checkMoveInsideChessboard(chessboard: Chessboard)(move: GenericMove): Option[GenericMove] = {
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
        case None => EmptyMoveStorage
        case Some(move) =>
          chessboard.get(move.dest) match {
            case None => EmptyMoveStorage.add(move.piece)(
              Seq(move
                .enableTag(TagIsMove)
                .enableTag(TagIsControl)))
            case Some(pieceDest) =>
              EmptyMoveStorage.add(move.piece)(
                Seq(addControlAndMoveIfTaking(move, pieceDest)))
          }
      }
    }
    directions.foldLeft(EmptyMoveStorage)((movesWithControls, direction) =>
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
    checkMoveInsideChessboard(chessboard)(move)
  }

  def moveMulGenWithControl(chessboard: Chessboard)(directions: Seq[Direction]): MovesStorage = {
    def moveMulWithControl(chessboard: Chessboard)(directionUnit: Direction): MovesStorage = {
      def _moveMul(chessboard: Chessboard)(direction: Direction): MovesStorage = {
        moveUnitWithoutControl(chessboard)(direction) match {
          case None => EmptyMoveStorage
          case Some(move) =>
            chessboard.get(move.dest) match {
              case None =>
                _moveMul(chessboard)(direction.compose(directionUnit)).add(move.piece)(
                  Seq(move
                    .enableTag(TagIsMove)
                    .enableTag(TagIsControl)))
              case Some(pieceDest) =>
                EmptyMoveStorage.add(move.piece)(
                  Seq(addControlAndMoveIfTaking(move, pieceDest)))
            }
        }
      }

      _moveMul(chessboard)(directionUnit)
    }

    directions.foldLeft(EmptyMoveStorage)((movesWithControls, direction) =>
      movesWithControls.add(moveMulWithControl(chessboard)(direction)))
  }
}