package dto

import model._
import model.board.RichSquare._
import model.board._

object PieceDto {

  def toPiece(color: Color, encoded: String): Piece = {
    require(encoded.length == 3)
    val symbol = encoded(0).toLower
    require("qkrnbp".contains(symbol))
    val position = encoded.substring(1).toSquare
    symbol match {
      case 'q' => QueenBoardImpl(color = color, position = position)
      case 'k' => KingBoardImpl(color = color, position = position)
      case 'r' => RookBoardImpl(color = color, position = position)
      case 'n' => KnightBoardImpl(color = color, position = position)
      case 'b' => BishopBoardImpl(color = color, position = position)
      case 'p' => PawnBoardImpl(color = color, position = position)
    }
  }

  def toStringShort(piece: Piece): String = {
    piece match {
      case _: Queen => "Q"
      case _: King => "K"
      case _: Rook => "R"
      case _: Bishop => "B"
      case _: Knight => "N"
      case _: Pawn => ""
    }
  }

  def toStringLong(piece: Piece): String = {
    val square = piece.position.toString.toLowerCase
    piece match {
      case _: Queen => s"Q$square"
      case _: King => s"K$square"
      case _: Rook => s"R$square"
      case _: Bishop => s"B$square"
      case _: Knight => s"N$square"
      case _: Pawn => s"p$square"
    }
  }
}
