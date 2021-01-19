package model.board

import dto.PieceDto
import model.Chessboard.MovesStorage
import model.GenericMove.{ TagMove, TagsMove, TagsMoveEmpty }
import model._

object MoveTag {
  final val isMove: Set[TagMove] = Set(TagIsMove)
  final val isControl: Set[TagMove] = Set(TagIsControl)
  // will be used for generation of moves by delta
  final val invalid: Set[TagMove] = Set(TagIsInvalid)
}
case object TagIsInvalid extends TagMove
case object TagIsMove extends TagMove
case object TagIsControl extends TagMove

object BaseMove {

  def showEasy(moves: Seq[GenericMove]) = {
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

sealed abstract class BaseMove(
  override val piece: Piece,
  override val dest: Square,
  override val takenPiece: Option[Piece] = None,
  override val tags: Set[TagMove] = Set()) extends GenericMove

case class Move(
  override val piece: Piece,
  override val dest: Square,
  override val takenPiece: Option[Piece] = None,
  override val tags: Set[TagMove] = Set())
  extends BaseMove(piece, dest, takenPiece, tags) {

  override def clearTags: GenericMove = this.copy(tags = Set())
  override def enableTag(tag: TagValue[GenericMove]): GenericMove =
    this.copy(tags = tags + tag)
  override def disableTag(tag: TagValue[GenericMove]): GenericMove =
    this.copy(tags = tags - tag)
}

case class Promotion(
  pawn: Pawn,
  newPiece: Piece,
  override val takenPiece: Option[Piece] = None,
  override val tags: TagsMove = TagsMoveEmpty)
  extends BaseMove(pawn, newPiece.position, takenPiece) {

  override def clearTags: GenericMove = this.copy(tags = TagsMoveEmpty)
  override def enableTag(tag: TagValue[GenericMove]): GenericMove =
    this.copy(tags = tags + tag)
  override def disableTag(tag: TagValue[GenericMove]): GenericMove =
    this.copy(tags = tags - tag)

  override def show(tools: Tools, moves: MovesStorage): String =
    s"${super.show(tools, moves)}=${PieceDto.toStringShort(newPiece)}"
}

case class Ep(
  pawn: Pawn,
  override val dest: Square,
  val takenPawn: Pawn,
  override val tags: TagsMove = TagsMoveEmpty)
  extends BaseMove(pawn, dest, Some(takenPawn)) {

  override def clearTags: GenericMove = this.copy(tags = TagsMoveEmpty)
  override def enableTag(tag: TagValue[GenericMove]): GenericMove =
    this.copy(tags = tags + tag)
  override def disableTag(tag: TagValue[GenericMove]): GenericMove =
    this.copy(tags = tags - tag)
}

case class SmallCastling(
  king: KingBoard,
  override val dest: Square,
  rookMove: BaseMove,
  override val tags: TagsMove = TagsMoveEmpty) extends BaseMove(king, dest) {

  override def clearTags: GenericMove = this.copy(tags = TagsMoveEmpty)
  override def enableTag(tag: TagValue[GenericMove]): GenericMove =
    this.copy(tags = tags + tag)
  override def disableTag(tag: TagValue[GenericMove]): GenericMove =
    this.copy(tags = tags - tag)

  override def show(tools: Tools, moves: MovesStorage): String = "o-o"
}
case class GreatCastling(
  king: KingBoard,
  override val dest: Square,
  rookMove: BaseMove,
  override val tags: TagsMove = TagsMoveEmpty) extends BaseMove(king, dest) {

  override def clearTags: GenericMove = this.copy(tags = TagsMoveEmpty)
  override def enableTag(tag: TagValue[GenericMove]): GenericMove =
    this.copy(tags = tags + tag)
  override def disableTag(tag: TagValue[GenericMove]): GenericMove =
    this.copy(tags = tags - tag)

  override def show(tools: Tools, moves: MovesStorage): String = "o-o-o"
}