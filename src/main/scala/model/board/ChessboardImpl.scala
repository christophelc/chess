package model.board

import com.typesafe.scalalogging.LazyLogging
import model.Chessboard.{ EndGame, MovesStorage }
import RichSquare.SquareXYFromString
import model.Square._
import model._
import model.board.PiecesSeq.EmptyPieces
import model.board.StorageImpl.emptyMoveStorage

case class ChessboardImpl(
  override val pieces: Pieces,
  override val moves: MovesStorage = StorageImpl.emptyMoveStorage,
  override val endGame: Option[EndGame] = None) extends Chessboard with LazyLogging {

  def +(piece: Piece): Chessboard = {
    require(pieces.atSquare(piece.position).isEmpty)
    ChessboardImpl(pieces = pieces.add(piece))
  }
  override def whoIsAttackingSquare(square: Square, controlsByOtherColor: MovesStorage): Seq[Piece] =
    controlsByOtherColor.toSeq.filter(_.dest == square).map(_.piece)

  override def findKing(color: Color): Piece = {
    pieces.king(color)
  }
  override def clear(square: Square): ChessboardImpl = pieces.atSquare(square) match {
    case Some(piece) => this.copy(pieces = pieces.sub(piece))
    case None => this
  }
  override def withEndGame(endGame: Option[EndGame]): Chessboard = this.copy(endGame = endGame)
  override def generateMoveWithControl(color: Color)(logBook: LogBook): MovesStorage =
    pieces.withColor(color).whereToGo(this)(logBook)

  override def generateMove(color: Color)(logBook: LogBook): MovesStorage = {
    val moves = generateMoveWithControl(color)(logBook).filterV(_.isTagged(TagIsMove))
    val king = findKing(color)

    val attackers = whoIsAttackingSquare(
      king.position,
      moves.filterV(_.isTagged(TagIsControl))
        .filterK(_.color == color.invert))
    val kingsMove: MovesStorage = emptyMoveStorage
      .add(
        moves
          .filterK(_ == king)
          .filterV(move => !isAttackedByColor(move.dest, color.invert)))

    val generatedMoves: MovesStorage = attackers.size match {
      case 0 => moves
      case 1 =>
        // move the king,
        // take the attacking piece or
        // hide check by moving a piece other than the king between the King and the attacking queen, bishop, rook

        //println("Check by: " + attackers.head.toString)
        val takeAttackingPiece: MovesStorage = moves.filterV(_.dest == attackers.head.position)
        val hideCheck: MovesStorage = attackers.head match {
          case _: Rook | _: Bishop | _: Queen =>
            val squaresForHidingCheck = king.position.squaresStrictlyBetween(attackers.head.position)
            moves.filterV(move => squaresForHidingCheck.contains(move.dest))
          case _ => emptyMoveStorage
        }
        kingsMove
          .add(takeAttackingPiece)
          .add(hideCheck)
      // double check: only the king can move
      case _ =>
        //println("Double check by: " + attackers.map(_.toString).mkString(" "))
        kingsMove
    }
    generatedMoves
      .filterV(move => move.piece match {
        case _: King =>
          // redundant if no check since we have already tested the king cannot move into a attacked square
          !play(move).updateControls(logBook).isAttackedByColor(move.dest, color.invert)
        case _ =>
          !play(move).updateControls(logBook).isAttackedByColor(king.position, color.invert)
      })
  }

  override def updateControls(logBook: LogBook): Chessboard =
    this.copy(moves = generateMoveWithControl(Black)(logBook).filterV(_.isTagged(TagIsControl))
      .add(generateMoveWithControl(White)(logBook).filterV(_.isTagged(TagIsControl))))

  def play(move: GenericMove): Chessboard = {
    def makeMove(move: GenericMove): Chessboard = {
      get(move.dest) match {
        case Some(piece) =>
          ChessboardImpl(pieces =
            pieces
              .sub(Seq(piece, move.piece))
              .add(move.piece.letsMove(move.dest)))
        case None =>
          ChessboardImpl(pieces =
            pieces
              .sub(move.piece)
              .add(move.piece.letsMove(move.dest)))
      }
    }
    move match {
      case SmallCastling(_, _, rookMove, _) => makeMove(move).play(rookMove)
      case GreatCastling(_, _, rookMove, _) => makeMove(move).play(rookMove)
      case Promotion(_, piece, _, _) => makeMove(move).clear(move.dest) + piece
      case Ep(_, _, takenPawn, _) => makeMove(move).clear(takenPawn.position)
      case _ => makeMove(move)
    }
  }
  override def equals(that: Any): Boolean =
    that match {
      case chessboard: Chessboard =>
        chessboard.pieces.containsSameElementAs(pieces)
      case _ => false
    }

  def isCheck(king: King): Boolean = isAttackedByColor(king.position, king.color.invert)
  def isAttackedByColor(square: Square, color: Color): Boolean =
    moves.filterV(_.isTagged(TagIsControl))
      .filterK(_.color == color)
      .filterV(_.dest == square).nonEmpty
  def isSmallCastlingAttacked(king: King): Boolean =
    isAttackedByColor(king.position.right, king.color.invert) &&
      isAttackedByColor(king.position.right.right, king.color.invert)

  def isGreatCastlingAttacked(king: King): Boolean =
    isAttackedByColor(king.position.left, king.color.invert) &&
      isAttackedByColor(king.position.left.left, king.color.invert) &&
      isAttackedByColor(king.position.left.left.left, king.color.invert)

  def canSmallCastling(history: LogBook, king: King): Boolean =
    isSmallCastlingAvailableNow(history, king) &&
      get(king.position.right).isEmpty &&
      get(king.position.right.right).isEmpty &&
      !isCheck(king) && !isSmallCastlingAttacked(king)

  def canGreatCastling(history: LogBook, king: King): Boolean =
    isGreatCastlingAvailableNow(history, king) &&
      get(king.position.left).isEmpty &&
      get(king.position.left.left).isEmpty &&
      get(king.position.left.left.left).isEmpty &&
      !isCheck(king) && !isGreatCastlingAttacked(king)

  override def toString: String = {
    val axisX = Seq("A", "B", "C", "D", "E", "F", "G", "H").mkString(" ")
    val axisY = Seq("1", "2", "3", "4", "5", "6", "7", "8")
    val board = (for (row <- 7 to 0 by -1) yield {
      val line = (for (col <- 0 to 7) yield get(SquareXY(Row(row), Col(col))).map(_.display)
        .getOrElse(SquareXY(Row(row), Col(col)).show)).mkString(" ")
      Seq(axisY(row), line, axisY(row)).mkString(" ")
    }).mkString("\n")
    Seq("  " + axisX, board, "  " + axisX).mkString("\n")
  }
}

object ChessboardImpl {
  def apply(): ChessboardImpl = {
    board.ChessboardImpl(initialState)
  }

  val empty: ChessboardImpl = ChessboardImpl(EmptyPieces)

  val initialState: Pieces = PiecesSeq.build(
    Seq(
      board.RookBoardImpl(White, "a1".toSquare),
      KnightBoardImpl(White, "b1".toSquare),
      BishopBoardImpl(White, "c1".toSquare),
      QueenBoardImpl(White, "d1".toSquare),
      board.KingBoardImpl(White, "e1".toSquare),
      board.BishopBoardImpl(White, "f1".toSquare),
      board.KnightBoardImpl(White, "g1".toSquare),
      board.RookBoardImpl(White, "h1".toSquare),
      board.RookBoardImpl(Black, "a8".toSquare),
      board.KnightBoardImpl(Black, "b8".toSquare),
      board.BishopBoardImpl(Black, "c8".toSquare),
      board.QueenBoardImpl(Black, "d8".toSquare),
      board.KingBoardImpl(Black, "e8".toSquare),
      board.BishopBoardImpl(Black, "f8".toSquare),
      board.KnightBoardImpl(Black, "g8".toSquare),
      board.RookBoardImpl(Black, "h8".toSquare)) ++
      (for (col <- 0 to 7) yield Seq(
        PawnBoardImpl(
          White,
          SquareXY(row = row2, col = Col(col))),
        PawnBoardImpl(
          Black,
          SquareXY(row = row7, col = Col(col)))))
      .flatten)
}