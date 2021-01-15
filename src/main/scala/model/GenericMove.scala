package model

import dto.PieceDto
import model.Chessboard.MovesStorage
import model.GenericMove.{ TagMove, TagsMove, TagsMoveEmpty }
import model.Piece._

object GenericMove {
  type TagMove = TagValue[GenericMove]
  type TagsMove = Set[TagMove]
  val TagsMoveEmpty: TagsMove = Set()
}
trait GenericMove extends Tagable[GenericMove] {
  val tags: TagsMove = TagsMoveEmpty

  val piece: Piece
  val dest: Square
  val takenPiece: Option[Piece] = None

  override def enableTag(tag: TagMove): GenericMove
  override def disableTag(tag: TagMove): GenericMove
  override def isTagged(tag: TagMove): Boolean = tags.contains(tag)
  def clearTags: GenericMove

  def showEasy(): String = {
    val taking = if (takenPiece.isDefined) "x" else ""
    val pawnIsTaking = piece match {
      case _: Pawn if takenPiece.isDefined => piece.position.toString.charAt(0).toLower.toString
      case _ => ""
    }
    s"$pawnIsTaking${PieceDto.toStringShort(piece)}$taking${dest.toString().toLowerCase}"
  }

  /**
   * Take into account check, taking
   * @param tools chessboard and logbook
   * @param moves moves to display
   * @return a pretty view of the annotated moves in string format
   */
  def show(tools: Tools, moves: MovesStorage): String = {
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
      case _: Pawn if takenPiece.isDefined => piece.position.toString.charAt(0).toLower.toString
      case _ => ""
    }
    // check if 2 pieces of same type can go at the same place (rook or bishop)
    val sameKind = moves.toSeq.filter(move =>
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