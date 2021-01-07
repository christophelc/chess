package model.board

import com.typesafe.scalalogging.LazyLogging
import model.Chessboard.EndGame
import model.RichSquare.SquareXYFromString
import model._
import model.board.BaseMove.{ EmptyMove, Moves }

case class ChessboardImpl(
  override val pieces: Pieces,
  override val controls: Moves = EmptyMove,
  override val endGame: Option[EndGame] = None) extends Chessboard with LazyLogging {

  override type Moves = BaseMove.Moves

  def +(piece: Piece): Chessboard = {
    require(pieces.atSquare(piece.position).isEmpty)
    ChessboardImpl(pieces = pieces.add(piece))
  }
  override def whoIsAttackingSquare(square: Square, controlsByOtherColor: Moves): Seq[Piece] =
    controlsByOtherColor.filter(_.dest == square).map(_.piece)

  override def findKing(color: Color): Piece = pieces.king(color)
  override def clear(square: Square): ChessboardImpl = pieces.atSquare(square) match {
    case Some(piece) => this.copy(pieces = pieces.sub(piece))
    case None => this
  }
  override def withEndGame(endGame: Option[EndGame]): Chessboard = this.copy(endGame = endGame)
  def get(row: Int, col: Int): Option[Piece] = get(SquareXY(row, col))
  override def generateMoveWithControl(color: Color)(logBook: LogBook): MovesWithControl =
    pieces.withColor(color).whereToGo(this)(logBook)

  override def generateMove(color: Color)(logBook: LogBook): Moves = {
    val moves = MovesWithControlImpl.convert(generateMoveWithControl(color)(logBook)).moves
    val king = findKing(color)

    val attackers = whoIsAttackingSquare(
      king.position,
      controls.filter(_.piece.color == color.invert))
    val kingsMove = moves
      .filter(_.piece == king)
      .filter(move => !isAttackedByColor(move.dest, color.invert))

    val generatedMoves = attackers.size match {
      case 0 => moves
      case 1 =>
        // move the king,
        // take the attacking piece or
        // hide check by moving a piece other than the king between the King and the attacking queen, bishop, rook

        //println("Check by: " + attackers.head.toString)
        val takeAttackingPiece = moves.filter(_.dest == attackers.head.position)
        val hideCheck = attackers.head match {
          case Rook(_, _) | Bishop(_, _) | Queen(_, _) =>
            val squaresForHidingCheck = king.position.squaresStrictlyBetween(attackers.head.position)
            moves.filter(move => squaresForHidingCheck.contains(move.dest))
          case _ => Nil
        }
        kingsMove ++ takeAttackingPiece ++ hideCheck
      // double check: only the king can move
      case _ =>
        //println("Double check by: " + attackers.map(_.toString).mkString(" "))
        kingsMove
    }
    generatedMoves
      .filter(move => move.piece match {
        case King(_, _) =>
          // redundant if no check since we have already tested the king cannot move into a attacked square
          !play(move).updateControls(logBook).isAttackedByColor(move.dest, color.invert)
        case _ =>
          !play(move).updateControls(logBook).isAttackedByColor(king.position, color.invert)
      })
  }

  override def updateControls(logBook: LogBook): Chessboard =
    this.copy(controls = MovesWithControlImpl.convert(generateMoveWithControl(Black)(logBook)).controls ++
      MovesWithControlImpl.convert(generateMoveWithControl(White)(logBook)).controls)

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
      case SmallCastling(_, _, rookMove) => makeMove(move).play(rookMove)
      case GreatCastling(_, _, rookMove) => makeMove(move).play(rookMove)
      case Promotion(_, piece, _) => makeMove(move).clear(move.dest) + piece
      case Ep(_, _, takenPawn) => makeMove(move).clear(takenPawn.position)
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
    controls.filter(_.piece.color == color).exists(_.dest == square)
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
      val line = (for (col <- 0 to 7) yield get(row, col).map(_.display).getOrElse(SquareXY(row, col).show)).mkString(" ")
      Seq(axisY(row), line, axisY(row)).mkString(" ")
    }).mkString("\n")
    Seq("  " + axisX, board, "  " + axisX).mkString("\n")
  }
}

object ChessboardImpl {
  def apply(): ChessboardImpl = {
    board.ChessboardImpl(initialState)
  }

  def convert(chessboard: Chessboard): ChessboardImpl =
    chessboard match { case chessboardImpl: ChessboardImpl => chessboardImpl }
  val empty: ChessboardImpl = ChessboardImpl(PiecesSeq(Nil))

  val initialState: Pieces = PiecesSeq(
    Seq(
      board.Rook(White, "a1".toSquare),
      Knight(White, "b1".toSquare),
      Bishop(White, "c1".toSquare),
      Queen(White, "d1".toSquare),
      board.King(White, "e1".toSquare),
      board.Bishop(White, "f1".toSquare),
      board.Knight(White, "g1".toSquare),
      board.Rook(White, "h1".toSquare),
      board.Rook(Black, "a8".toSquare),
      board.Knight(Black, "b8".toSquare),
      board.Bishop(Black, "c8".toSquare),
      board.Queen(Black, "d8".toSquare),
      board.King(Black, "e8".toSquare),
      board.Bishop(Black, "f8".toSquare),
      board.Knight(Black, "g8".toSquare),
      board.Rook(Black, "h8".toSquare)) ++
      (for (col <- 0 to 7) yield Seq(
        Pawn(
          White,
          SquareXY(row = 1, col = col)),
        Pawn(
          Black,
          SquareXY(row = 6, col = col))))
      .flatten)
}