package dto

import model.Square._
import model.board._
import model.{ Chessboard, board, _ }
import model.board.RichSquare._

import scala.util.matching.Regex

case class ChessboardDto(encoded: String)

trait Codec {
  def encode(chessboard: Chessboard): ChessboardDto
  def decode(chessboardDto: ChessboardDto): Chessboard
}
object SimpleChessboardCodec {
  val colorSeparator = ";"
}
class SimpleChessboardCodec extends Codec {

  import SimpleChessboardCodec._

  private def encodeOneColor(pieces: Seq[Piece]): String = {
    pieces.sortBy(piece => s"${PieceDto.toStringLong(piece)}${piece.position.toString}")
      .map(PieceDto.toStringLong)
      .mkString("")
  }

  override def encode(chessboard: Chessboard): ChessboardDto = {
    val piecesPerColor: Map[Color, Seq[Piece]] = chessboard.pieces.toSeq.groupBy(_.color)
    ChessboardDto(encodeOneColor(piecesPerColor(White)) +
      colorSeparator +
      encodeOneColor(piecesPerColor(Black)))
  }

  override def decode(chessboardDto: ChessboardDto): Chessboard = {
    val piecesPerColor = chessboardDto.encoded.toLowerCase.split(colorSeparator)
    require(piecesPerColor.length == 2)

    val piecesWhiteDto = piecesPerColor(0)
    val piecesBlackDto = piecesPerColor(1)
    require(piecesWhiteDto.length % 3 == 0)
    require(piecesBlackDto.length % 3 == 0)
    val piecesWhite = ChessboardImplConfiguration$Piece.buildPieces(
      piecesWhiteDto
        .toSeq
        .grouped(3)
        .map(pieceDto => PieceDto.toPiece(White, pieceDto.toString())).toSeq)
    val piecesBlack = ChessboardImplConfiguration$Piece.buildPieces(
      piecesBlackDto
        .toSeq
        .grouped(3)
        .map(pieceDto => PieceDto.toPiece(Black, pieceDto.toString())).toSeq)
    ChessboardImplConfiguration$Piece(piecesWhite.union(piecesBlack))
  }
}

class FENCodec {
  private def readLine(row: Row, line: String): Pieces = {
    case class Acc(col: Col = colA, pieces: Pieces = ChessboardImplConfiguration$Piece.emptyPieces)
    line.toCharArray.foldLeft(Acc())((acc, c) => {
      val square = SquareXY(row = row, col = acc.col)
      if (c.isLetter) {
        val color: Color = if (c.toLower == c) Black else White
        val piece: Piece = c.toLower match {
          case 'p' => PawnBoard(color, square)
          case 'r' => RookBoard(color, square)
          case 'b' => BishopBoard(color, square)
          case 'n' => KnightBoard(color, square)
          case 'q' => QueenBoard(color, square)
          case 'k' => KingBoard(color, square)
        }
        acc.copy(col = acc.col.add(1), pieces = acc.pieces.add(piece))
      } else {
        acc.copy(col = acc.col.add(c - '0'))
      }
    }).pieces
  }

  private def pieceToString(piece: Piece): String = {
    piece match {
      case QueenBoard(_, _) => "Q"
      case KingBoard(_, _) => "K"
      case RookBoard(_, _) => "R"
      case BishopBoard(_, _) => "B"
      case KnightBoard(_, _) => "N"
      case PawnBoard(_, _) => "P"
    }
  }

  private def castling(tools: Tools): String = {
    val castle = Seq(
      (!tools.logBook.smallCastlingForbiddenWhite, "K"),
      (!tools.logBook.greatCastlingForbiddenWhite, "Q"),
      (!tools.logBook.smallCastlingForbiddenBlack, "k"),
      (!tools.logBook.greatCastlingForbiddenBlack, "q"))
      .filter(_._1 == true)
      .map(_._2)
      .mkString("")
    if (castle == "") "-" else castle
  }
  def encode(game: ChessGame): ChessboardDto = {
    val lines: String = (for (row <- 7 to 0 by -1) yield {
      (for (col <- 0 to 7) yield {
        game.tools.chessboard.get(SquareXY(row = Row(row), col = Col(col))).map(piece => {
          val s = pieceToString(piece)
          if (piece.color == White) s.toUpperCase() else s.toLowerCase()
        }).getOrElse(" ")
      }).mkString("")
    }).mkString("/") + "/"

    case class Acc(lines: Seq[String] = Nil, currentLine: String = "", emptySquare: Int = 0)
    val enc = lines.foldLeft(Acc())((acc, c) => {
      c match {
        case ' ' => acc.copy(emptySquare = acc.emptySquare + 1)
        case '/' if (acc.currentLine.isEmpty) =>
          acc.copy(
            lines = acc.lines :+ (if (acc.lines.size < 8) "8/" else "8"),
            currentLine = "",
            emptySquare = 0)
        case '/' =>
          val n = if (acc.emptySquare == 0) "" else acc.emptySquare.toString
          acc.copy(
            lines = acc.lines :+ (if (acc.lines.size < 8)
              s"${acc.currentLine}$n/"
            else
              s"${acc.currentLine}$n"),
            currentLine = "",
            emptySquare = 0)
        case piece if acc.emptySquare == 0 =>
          acc.copy(currentLine = acc.currentLine + piece)
        case piece =>
          acc.copy(
            currentLine = acc.currentLine + (acc.emptySquare.toString + piece),
            emptySquare = 0)
      }
    })
    val chessboard = enc.lines.mkString("").dropRight(1)
    val whichPlayer = game.whichPlayerTurn match {
      case White => "w"
      case Black => "b"
    }
    val castle = castling(game.tools)
    val ep = game.tools.logBook.epForLastMove.map(_.toString).getOrElse("-")
    val plyLastCapture = game.tools.logBook.plyLastCaptureOrPawnMove.toString
    val nextMove = game.tools.logBook.nextMove.toString
    ChessboardDto(s"$chessboard $whichPlayer $castle $ep $plyLastCapture $nextMove")
  }

  def decode(chessboardDto: ChessboardDto): ChessGame = {
    val s = (for (i <- 0 to 7) yield "([rnbqkpRNBQKP1-8]+)").mkString("/") + """\s([b|w])\s(-|[K|Q|k|q]{1,4})\s(-|[a-h][1-8])\s(\d+\s\d+)$"""
    val pattern = new Regex(s"^$s")
    val pattern(line8, line7, line6, line5, line4, line3, line2, line1, whichPlayer, castle, ep, plyCaptureAndMove) = chessboardDto.encoded
    val pieces = (for ((line, idx) <- Seq(line1, line2, line3, line4, line5, line6, line7, line8).zipWithIndex)
      yield readLine(row = Row(idx), line).toSeq).flatten
    ChessGame(
      playerBlack = PlayerReal("Black"),
      playerWhite = PlayerReal("White"),
      whichPlayerTurn = if (whichPlayer == "w") White else Black,
      tools = Tools(
        chessboard = board.ChessboardImplConfiguration$Piece(
          pieces = ChessboardImplConfiguration$Piece.buildPieces(pieces)),
        logBook = LogBook(
          smallCastlingForbiddenWhite = !castle.contains("K"),
          greatCastlingForbiddenWhite = !castle.contains("Q"),
          smallCastlingForbiddenBlack = !castle.contains("k"),
          greatCastlingForbiddenBlack = !castle.contains("q"),
          plyLastCaptureOrPawnMove = plyCaptureAndMove.split(" ")(0).toInt,
          epForLastMove = if (ep == "-") None else Some(ep.toSquare),
          _nextMove = plyCaptureAndMove.split(" ")(1).toInt)),
      timer = Infinite)
  }

}
