package model

import model.Piece.idKing
import model.board.BaseMove.{ EmptyMove, Moves }
import model.board._

/**
 *
 * @param moves
 * @param initialPosition
 * @param whichPlayerStart
 * @param smallCastlingForbiddenWhite when entering a position by hand, if the king is at its initial position,
 *                              indicate if it can do the small castling. This is not taken into account if the
 *                              king is not on the right position (e1 for white, e8 for black)
 * @param greatCastlingForbiddenWhite when entering a position by hand, if the king is at its initial position,
 *                               indicate if it can do the great castling. This is not taken into account if the
 *                               king is not on the right position (e1 for white, e8 for black)
 * @param smallCastlingForbiddenBlack
 * @param greatCastlingForbiddenBlack
 * @param epForLastMove
 * @param plyLastCaptureOrPawnMove
 * @param nextMove
 */
case class LogBook(
  moves: Seq[GenericMove] = Seq(),
  initialPosition: Chessboard = ChessboardImpl(),
  val whichPlayerStart: Color = White,
  val smallCastlingForbiddenWhite: Boolean = false,
  val greatCastlingForbiddenWhite: Boolean = false,
  val smallCastlingForbiddenBlack: Boolean = false,
  val greatCastlingForbiddenBlack: Boolean = false,
  val epForLastMove: Option[Square] = None,
  val plyLastCaptureOrPawnMove: Int = 0,
  nextMove: Int = 1) {

  require(nextMove >= 1)

  def add(move: GenericMove): LogBook = {
    this.copy(moves = moves :+ move)
      .updatePlyLastCapture(move)
  }

  private def updatePlyLastCapture(move: GenericMove): LogBook = {
    move.takenPiece match {
      case None => this.copy(plyLastCaptureOrPawnMove = plyLastCaptureOrPawnMove + 1)
      case Some(_) => this.copy(plyLastCaptureOrPawnMove = 0)
    }
  }

  def isSmallCastlingAvailable(king: Piece): Boolean =
    king.id == idKing &&
      (!smallCastlingForbiddenWhite && king.color == White ||
        !smallCastlingForbiddenBlack && king.color == Black) &&
        KingValue.initialPosition(king.color) == king.position && // for position manually set
        !pieceHasMoved(king)

  def isGreatCastlingAvailable(king: Piece): Boolean =
    king.id == idKing &&
      (!greatCastlingForbiddenWhite && king.color == White ||
        !greatCastlingForbiddenBlack && king.color == Black) &&
        !pieceHasMoved(king) &&
        KingValue.initialPosition(king.color) == king.position

  // FIXME: to be claimed by a player. or >= 75 moves
  def isNull50movesRule: Boolean =
    moves.length - plyLastCaptureOrPawnMove >= 100

  // FIXME: 3x and not 1x
  def isNullByRepetition3x: Boolean = {
    def reduceMoves(moves: Seq[GenericMove]): Seq[GenericMove] = {
      moves match {
        case Nil => Nil
        case head :: tail =>
          tail.find(move =>
            move.dest == head.piece.position &&
              move.piece.color == head.piece.color &&
              move.piece.display == head.piece.display) match {
            case None => moves
            case Some(move) => reduceMoves(tail.filter(_ != move))
          }
      }
    }
    def lastRelevantPositions(m: Seq[GenericMove]): Seq[GenericMove] = {
      m match {
        case Nil => Nil
        case head :: tail =>
          (head.piece, head.takenPiece) match {
            case (Pawn(_, _), _) | (_, Some(_)) => Seq(head)
            case _ => head +: lastRelevantPositions(tail)
          }
      }
    }
    false // TODO
    //val lastMoves = lastRelevantPositions(moves.reverse)
    //isNullByRepetition3x(moves)
  }

  def pieceHasMoved(piece: Piece): Boolean =
    moves.map(_.piece).contains(piece)

  def kingCastled(king: Piece): Boolean =
    moves.exists {
      case smallCastling: SmallCastling if smallCastling.piece == king => true
      case greatCastling: GreatCastling if greatCastling.piece == king => true
      case _ => false
    }

  override def toString(): String = {
    val tools = Tools(
      chessboard = this.initialPosition,
      logBook = LogBook())
    val plyInitial = (nextMove - 1) * 2 + (if (whichPlayerStart == White) 0 else 1)
    case class Acc(
      tools: Tools = tools,
      ply: Int = plyInitial,
      whichPlayer: Color = whichPlayerStart,
      moves: Seq[String] = Nil)
    moves.foldLeft(Acc())((acc, move) => {
      val movesAvailables = ChessboardImpl.convert(tools.chessboard).generateMove(acc.whichPlayer)(tools.logBook)
      val moveToString = move.show(acc.tools, movesAvailables)
      (if (acc.ply % 2 == 0) {
        val idx: Int = acc.ply / 2 + 1
        acc.copy(moves = acc.moves :+ s"$idx. $moveToString")
      } else {
        acc.copy(moves = acc.moves :+ s" - $moveToString\n")
      }).copy(
        whichPlayer = acc.whichPlayer.invert,
        tools = acc.tools.playAndUpdateControls(move),
        ply = acc.ply + 1)
    }).moves.mkString(" ")
  }
}
