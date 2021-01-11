package model.board

import dto.PieceDto
import model.Piece.{ idBishop, idRook }
import model.board.BaseMove.Moves
import model.{ Color, GenericMove, Piece, Square, Tools, White }

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

object MovesManager {
  def build(piece: Piece, moves: Seq[GenericMove]): MovesManager =
    MovesManager(Map(piece -> moves))

  def build(moves: Seq[GenericMove]): MovesManager =
    MovesManager(moves.groupBy(_.piece))
}

case class MovesManager(moves: Map[Piece, Seq[GenericMove]]) {
  def toSeq: Seq[GenericMove] = moves.values.toSeq.flatten
  def isEmpty: Boolean = moves.isEmpty
  def count: Int = moves.map(_._2.length).sum
  def filterColor(color: Color): MovesManager =
    this.copy(moves = moves.filter {
      case (piece, v) => piece.color == color
    })
  def filterDest(dest: Square): MovesManager =
    this.copy(moves = moves.map {
      case (k, v) => k -> v.filter(_.dest == dest)
    }.filter(_._2.nonEmpty))
  def filterPiece(piece: Piece): MovesManager =
    this.copy(moves = moves.filter(_._1 == piece))
  // the most efficent way found
  def add(otherMoves: MovesManager): MovesManager = this.copy(moves =
    otherMoves.moves.toSeq.foldLeft(moves)((acc, newMoves) => {
      val piece = newMoves._1
      if (acc.contains(piece)) {
        (acc - piece) ++ Map(piece -> (acc(piece) ++ newMoves._2))
      } else {
        acc ++ Map(newMoves)
      }
    }))
  def add(move: GenericMove): MovesManager = add(MovesManager(moves = Map(move.piece -> Seq(move))))
}

object BaseMove {
  type Moves = MovesManager
  val EmptyMove: Moves = MovesManager(Map[Piece, Seq[GenericMove]]())

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