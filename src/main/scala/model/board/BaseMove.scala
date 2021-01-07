package model.board

import dto.PieceDto
import model.Piece.{ idRook, idBishop }
import model.board.BaseMove.Moves
import model.{ GenericMove, Piece, Square, Tools, White }

sealed abstract class BaseMove(
  override val piece: Piece,
  override val dest: Square,
  override val takenPiece: Option[Piece] = None) extends GenericMove {

  override def showEasy(): String = {
    val taking = if (takenPiece.isDefined) "x" else ""
    val pawnIsTaking = piece match {
      case Pawn(_, _) if takenPiece.isDefined => piece.position.toString.charAt(0).toLower.toString
      case _ => ""
    }
    s"$pawnIsTaking${PieceDto.toStringShort(piece)}$taking${dest.toString().toLowerCase}"
  }
  override def show(tools: Tools, moves: Moves): String = {
    val taking = if (takenPiece.isDefined) "x" else ""
    val check = piece match {
      case _: King => ""
      case piece: Piece =>
        val kingOpponent = tools.chessboard.findKing(piece.color.invert)
        if (tools.chessboard.play(this)
          .updateControls(tools.logBook)
          .isAttackedByColor(kingOpponent.position, piece.color)) {
          "+"
        } else ""
    }
    val pawnIsTaking = piece match {
      case Pawn(_, _) if takenPiece.isDefined => piece.position.toString.charAt(0).toLower.toString
      case _ => ""
    }
    // check if 2 pieces of same type can go at the same place (rook or bishop)
    val sameKind = moves.filter(move =>
      (piece.id == idRook || piece.id == idBishop) &&
        move.piece.id == piece.id &&
        move.dest == dest)
    val whichPiece = if (sameKind.size >= 2) {
      // with 2 rooks (there can be more !)
      // case 1: r--t--r  t: target, r: rook
      // case 2: the same but vertical
      // case 3:  r
      //          |
      //          t---r
      // to simplify, we specify both the row and the column
      piece.position.toString.toLowerCase()
    } else ""
    s"$pawnIsTaking${PieceDto.toStringShort(piece)}$whichPiece${taking}${dest.toString().toLowerCase}$check"
  }
}

object BaseMove {
  type Moves = Seq[GenericMove]
  val EmptyMove: Seq[GenericMove] = Nil

  def showEasy(moves: Moves) = {
    case class Acc(show: String = "", ply: Int = 0)
    moves match {
      case Nil => ""
      case seq => {
        val color = seq.head.piece.color
        val ply = if (color == White) 0 else 1
        seq.foldLeft(Acc(ply = ply))((acc, move) =>
          (if (acc.ply % 2 == 0) {
            val idx = acc.ply / 2 + 1
            acc.copy(show = s"${acc.show}$idx. ${move.showEasy()}")
          } else
            acc.copy(show = s"${acc.show} - ${move.showEasy()}\n")).copy(ply = acc.ply + 1))
      }.show
    }
  }
}

case class Move(override val piece: Piece, override val dest: Square, override val takenPiece: Option[Piece] = None)
  extends BaseMove(piece, dest, takenPiece)

case class Promotion(pawn: Pawn, newPiece: Piece, override val takenPiece: Option[Piece] = None)
  extends BaseMove(pawn, newPiece.position, takenPiece) {

  override def show(tools: Tools, moves: Moves): String =
    s"${super.show(tools, moves)}=${PieceDto.toStringShort(newPiece)}"
}

case class Ep(pawn: Pawn, override val dest: Square, val takenPawn: Pawn)
  extends BaseMove(pawn, dest, Some(takenPawn))

case class SmallCastling(
  king: King,
  override val dest: Square,
  rookMove: BaseMove) extends BaseMove(king, dest) {
  override def show(tools: Tools, moves: Moves): String = "o-o"
}
case class GreatCastling(
  king: King,
  override val dest: Square,
  rookMove: BaseMove) extends BaseMove(king, dest) {
  override def show(tools: Tools, moves: Moves): String = "o-o-o"
}