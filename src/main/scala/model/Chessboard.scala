package model

import dto.CodecBitmap
import model.Chessboard.EndGame
import config.ConfigurationChessboard.MovesStorage
import model.Piece.{ PieceId, idRook }

trait ChessboardInit[K, V <: Piece] {
  def buildPieces(pieces: Seq[V]): Pieces[K, V]
  val emptyChessboard: Chessboard
  val emptyPieces: Pieces[K, V]
  def emptyMove: MovesStorage
  val initialState: Pieces[K, V]
}

trait Chessboard {
  val moves: MovesStorage
  def pieces: Pieces[PieceId, Piece]
  val endGame: Option[EndGame] = None

  def toLogPosition: LogPosition = LogPosition(CodecBitmap.encode(this))
  def findKing(color: Color): Piece
  def isNullByInsufficientMaterial: Boolean =
    if (pieces.count >= 4 ||
      pieces.pawns.nonEmpty ||
      pieces.rooks.nonEmpty ||
      pieces.queens.nonEmpty) {
      false
    } else {
      val piecesToConsider = pieces.bishops.union(pieces.knights)
      piecesToConsider.withColor(Black).count <= 1 ||
        piecesToConsider.withColor(White).count <= 1
    }
  def get(square: Square): Option[Piece] = pieces.atSquare(square)
  def isAttackedByColor(square: Square, color: Color): Boolean
  def whoIsAttackingSquare(square: Square, controlsByOtherColor: MovesStorage): Seq[Piece]
  def updateControls(logBook: LogBook): Chessboard
  def play(move: GenericMove): Chessboard
  def clear(square: Square): Chessboard
  def +(piece: Piece): Chessboard
  def generateMoveWithControl(color: Color)(logBook: LogBook): MovesStorage
  def generateMove(color: Color)(logBook: LogBook): MovesStorage
  def withEndGame(endGame: Option[EndGame]): Chessboard
  def isCheck(color: Color): Boolean =
    isAttackedByColor(findKing(color).position, color.invert)
  def canSmallCastling(history: LogBook, king: King): Boolean
  def canGreatCastling(history: LogBook, king: King): Boolean
  def isSmallCastlingAvailableNow(logBook: LogBook, king: Piece): Boolean = {
    logBook.isSmallCastlingAvailable(king) &&
      get(king.position.right.right.right).exists(rook =>
        rook.id == idRook && rook.color == king.color && !logBook.pieceHasMoved(rook))
  }

  def isGreatCastlingAvailableNow(logBook: LogBook, king: Piece): Boolean = {
    logBook.isGreatCastlingAvailable(king) &&
      get(king.position.left.left.left.left).exists(rook =>
        rook.id == idRook && rook.color == king.color && !logBook.pieceHasMoved(rook))
  }

  def isCastleAvailable(logBook: LogBook, color: Color): Boolean = {
    val king: Piece = findKing(color)
    logBook.isSmallCastlingAvailable(king) || logBook.isGreatCastlingAvailable(king)
  }

  def isCastleAvailableNow(logBook: LogBook, color: Color): Boolean = {
    val king: Piece = findKing(color)
    isSmallCastlingAvailableNow(logBook, king) || isGreatCastlingAvailableNow(logBook, king)
  }
}

object Chessboard {
  trait EndGame
  case object EndGame50MoveNoTakenPieceNoPawnMove extends EndGame
  case object EndGameByRepetition extends EndGame
  case object EndGameByInsufficientMaterial extends EndGame
  case object EndgameByCheckPat extends EndGame
  case object EndgameByCheckMat extends EndGame

}
