package dto

import model._
import RichSquare._
import model.board.{ Bishop, King, Knight, Pawn, Queen, Rook }

object PieceDto {

  def toPiece(color: Color, encoded: String): Piece = {
    require(encoded.length == 3)
    val symbol = encoded(0).toLower
    require("qkrnbp".contains(symbol))
    val position = encoded.substring(1).toSquare
    symbol match {
      case 'q' => Queen(color = color, position = position)
      case 'k' => King(color = color, position = position)
      case 'r' => Rook(color = color, position = position)
      case 'n' => Knight(color = color, position = position)
      case 'b' => Bishop(color = color, position = position)
      case 'p' => Pawn(color = color, position = position)
    }
  }

  def toStringShort(piece: Piece): String = {
    piece match {
      case Queen(_, _) => "Q"
      case King(_, _) => "K"
      case Rook(_, _) => "R"
      case Bishop(_, _) => "B"
      case Knight(_, _) => "N"
      case Pawn(_, _) => ""
    }
  }

  def toStringLong(piece: Piece): String = {
    val square = piece.position.toString.toLowerCase
    val pieceDto = toStringShort(piece)
    piece match {
      case Queen(_, _) => s"Q$square"
      case King(_, _) => s"K$square"
      case Rook(_, _) => s"R$square"
      case Bishop(_, _) => s"B$square"
      case Knight(_, _) => s"N$square"
      case Pawn(_, _) => s"p$square"
    }
  }
}
