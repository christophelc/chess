package model.board

import model.Chessboard.MovesStorage
import model.{ Black, Chessboard, Color, Direction, GenericMove, LogBook, Pawn, Piece, Square }
import model.board.MoveTag.isMove
import model.data.StorageMap.EmptyMoveStorage

case class PawnBoard(
  override val color: Color,
  override val position: Square) extends PieceBoard(color, position) with Pawn {

  override def letsMove(dest: Square): Piece = this.copy(position = dest)
  override def display: String = if (color == Black) "♙" else "♟"

  private def promotion(chessboard: Chessboard)(move: GenericMove): Seq[Promotion] = Seq(
    Promotion(
      pawn = this,
      newPiece = QueenBoard(color = this.color, position = move.dest),
      takenPiece = chessboard.get(move.dest),
      tags = move.tags ++ isMove),
    Promotion(
      pawn = this,
      newPiece = KnightBoard(color = this.color, position = move.dest),
      takenPiece = chessboard.get(move.dest),
      tags = move.tags ++ isMove),
    Promotion(
      pawn = this,
      newPiece = BishopBoardImpl(color = this.color, position = move.dest),
      takenPiece = chessboard.get(move.dest),
      tags = move.tags ++ isMove),
    Promotion(
      pawn = this,
      newPiece = RookBoard(color = this.color, position = move.dest),
      takenPiece = chessboard.get(move.dest),
      tags = move.tags ++ isMove))

  override def whereToGo(chessboard: Chessboard)(logBook: LogBook): MovesStorage = {

    val (verticalDirection, pawnInitialRow) = (Piece.verticalDirection(color), Piece.pawnInitialRow(color))
    val diagonalLeft = verticalDirection.left
    val diagonalRight = verticalDirection.right

    val verticalMove1 = moveUnitWithoutControl(chessboard)(verticalDirection)
      .filter(move => chessboard.get(move.dest).isEmpty)
    val verticalMove2 = verticalMove1
      .filter(_ => position.whichRow == pawnInitialRow)
      .flatMap(_ => moveUnitWithoutControl(chessboard)(verticalDirection.compose(verticalDirection))
        .filter(move => chessboard.get(move.dest).isEmpty))

    val diagLeft = moveUnitWithoutControl(chessboard)(diagonalLeft)
      .map(move =>
        if (chessboard.get(move.dest).exists(_.color == color.invert))
          move.enableTag(TagIsControl).enableTag(TagIsMove)
        else
          move.enableTag(TagIsControl).disableTag(TagIsMove))
    val diagRight = moveUnitWithoutControl(chessboard)(diagonalRight)
      .map(move =>
        if (chessboard.get(move.dest).exists(_.color == color.invert))
          move.enableTag(TagIsControl).enableTag(TagIsMove)
        else
          move.enableTag(TagIsControl).disableTag(TagIsMove))

    val ep = logBook.moves.lastOption.filter {
      move: GenericMove =>
        move.piece match {
          case pawn: Pawn if math.abs(pawn.position.whichRow.minus(move.dest.whichRow)) == 2 &&
            move.dest.whichRow == this.position.whichRow &&
            math.abs(this.position.whichCol.minus(pawn.position.whichCol)) == 1 => true
          case _ => false
        }
    }.map(lastMovePawn2squares =>
      Ep(
        pawn = this,
        dest = position.shift(Direction(
          vertical = verticalDirection.vertical,
          horizontal = lastMovePawn2squares.piece.position.whichCol.minus(this.position.whichCol))),
        takenPawn = PawnBoard(lastMovePawn2squares.piece.color, lastMovePawn2squares.dest),
        tags = isMove))
    // cannot be a promotion
    val movesOnly = Seq(verticalMove2, ep).flatten
    // check if there is a promotion
    val moveOrPromotions: Seq[GenericMove] =
      (verticalMove1.toSeq ++ diagLeft.toSeq ++ diagRight.toSeq).flatMap(move =>
        if (move.dest.isRowLimit && move.isTagged(TagIsMove)) {
          promotion(chessboard)(move)
        } else {
          Seq(move)
        })
    EmptyMoveStorage
      .add(this)(movesOnly ++ moveOrPromotions)
  }
}