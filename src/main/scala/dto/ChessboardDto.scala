package dto

import model.Chessboard.NoPiece
import model.board.{ Bishop, ChessboardImpl, King, Knight, Pawn, PiecesSeq, Queen, Rook }
import model.{ Chessboard, board, _ }

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

  private def encodeOneColor(pieces: Pieces): String = {
    pieces.list.sortBy(piece => s"${PieceDto.toStringLong(piece)}${piece.position.toString}")
      .map(PieceDto.toStringLong)
      .mkString("")
  }

  override def encode(chessboard: Chessboard): ChessboardDto = {
    val piecesPerColor: Map[Color, Pieces] = chessboard.pieces.groupByColor
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
    val piecesWhite = PiecesSeq(piecesWhiteDto.toSeq.grouped(3).map(pieceDto => PieceDto.toPiece(White, pieceDto.toString())).toSeq)
    val piecesBlack = PiecesSeq(piecesBlackDto.toSeq.grouped(3).map(pieceDto => PieceDto.toPiece(Black, pieceDto.toString())).toSeq)
    ChessboardImpl(piecesWhite.union(piecesBlack))
  }
}

class FENCodec {
  private def readLine(row: Int, line: String): Pieces = {
    case class Acc(col: Int = 0, pieces: Pieces = PiecesSeq(NoPiece))
    line.toCharArray.foldLeft(Acc())((acc, c) => {
      val square = SquareXY(row = row, col = acc.col)
      if (c.isLetter) {
        val color: Color = if (c.toLower == c) Black else White
        val piece: Piece = c.toLower match {
          case 'p' => Pawn(color, square)
          case 'r' => Rook(color, square)
          case 'b' => Bishop(color, square)
          case 'n' => Knight(color, square)
          case 'q' => Queen(color, square)
          case 'k' => King(color, square)
        }
        acc.copy(col = acc.col + 1, pieces = acc.pieces.add(piece))
      } else {
        acc.copy(col = acc.col + c - '0')
      }
    }).pieces
  }

  private def pieceToString(piece: Piece): String = {
    piece match {
      case Queen(_, _) => "Q"
      case King(_, _) => "K"
      case Rook(_, _) => "R"
      case Bishop(_, _) => "B"
      case Knight(_, _) => "N"
      case Pawn(_, _) => "P"
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
        game.tools.chessboard.get(SquareXY(row = row, col = col)).map(piece => {
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
    val plyLastCapture = game.tools.logBook.plyLastCapture.toString
    val nextMove = game.tools.logBook.nextMove.toString
    ChessboardDto(s"$chessboard $whichPlayer $castle $ep $plyLastCapture $nextMove")
  }

  def decode(chessboardDto: ChessboardDto): ChessGame = {
    val s = (for (i <- 0 to 7) yield "([rnbqkpRNBQKP1-8]+)").mkString("/") + """\s([b|w])\s(-|[K|Q|k|q]{1,4})\s(-|[a-h][1-8])\s(\d+\s\d+)$"""
    val pattern = new Regex(s"^$s")
    val pattern(line8, line7, line6, line5, line4, line3, line2, line1, whichPlayer, castle, ep, plyCaptureAndMove) = chessboardDto.encoded
    val pieces = (for ((line, idx) <- Seq(line1, line2, line3, line4, line5, line6, line7, line8).zipWithIndex) yield readLine(row = idx, line).list).flatten
    ChessGame(
      playerBlack = PlayerReal("Black"),
      playerWhite = PlayerReal("White"),
      whichPlayerTurn = if (whichPlayer == "w") White else Black,
      tools = Tools(
        chessboard = board.ChessboardImpl(
          pieces = PiecesSeq(pieces)),
        logBook = LogBook(
          smallCastlingForbiddenWhite = !castle.contains("K"),
          greatCastlingForbiddenWhite = !castle.contains("Q"),
          smallCastlingForbiddenBlack = !castle.contains("k"),
          greatCastlingForbiddenBlack = !castle.contains("q"),
          plyLastCapture = plyCaptureAndMove.split(" ")(0).toInt,
          nextMove = plyCaptureAndMove.split(" ")(1).toInt)),
      timer = Infinite)
  }

}
