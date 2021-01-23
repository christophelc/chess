package model.bitboard

import model._

object Bitboard {
  def build(pieces: Seq[Piece]): Bitboard =
    pieces.foldLeft(Bitboard())((acc, piece) => acc.addPiece(piece))
}

case class Bitboard(white: BitboardCodec = BitboardCodec(), black: BitboardCodec = BitboardCodec()) {

  def isEmpty(color: Color)(square: Square): Boolean =
    color match {
      case White => white.isEmpty(square)
      case Black => black.isEmpty(square)
    }

  def play(move: GenericMove): Bitboard = move.piece.color match {
    case White => this.copy(white = white.movePiece(move))
    case Black => this.copy(black = black.movePiece(move))
  }

  def addPiece(piece: Piece): Bitboard = piece.color match {
    case White => this.copy(white = white.addPiece(piece.position))
    case Black => this.copy(black = black.addPiece(piece.position))
  }
}
