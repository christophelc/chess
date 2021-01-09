package dto

import model.board.RichSquare.SquareXYFromString
import model.board.BaseMove.Moves
import model.board.ChessboardImpl
import model.{ Black, Color, GenericMove, LogBook, Tools, White }

import scala.util.Try
import scala.util.matching.Regex

case class TwoMovesDto(id: Int, white: Option[String], black: Option[String]) {
  require(white.isDefined || black.isDefined)
}

object MoveDto {
  def orNone(s: String): Option[String] = Try(Option(s)).getOrElse(None)
  def read(move: String): TwoMovesDto = {
    val moveRegex: Regex = "([0-9]+).([a-zA-Z0-9=]+)?-([a-zA-Z0-9=-]+)?".r

    val moveRegex(idx, whiteMove, blackMove) = move.replace(" ", "")
    require(whiteMove != null || blackMove != null)
    TwoMovesDto(idx.toInt, orNone(whiteMove), orNone(blackMove))
  }

  def readMultiline(moves: String): (Color, Seq[String]) = {
    val movesDto: Seq[TwoMovesDto] = moves.split("\\n").map(move => MoveDto.read(move))
    require(movesDto.nonEmpty)
    require(movesDto.head.black.isDefined)
    require(movesDto.tail.isEmpty || movesDto.last.white.isDefined)
    for ((moveDto, idx) <- movesDto.zipWithIndex) {
      require(moveDto.id == idx + 1)
    }
    val colorToPlay = if (movesDto.head.white.isDefined) White else Black
    (colorToPlay, movesDto.flatMap(move => Seq(move.white, move.black).flatten))
  }

  private def encode(tools: Tools, color: Color)(move: String): Either[String, GenericMove] = {
    val moves = ChessboardImpl.convert(tools.chessboard).generateMove(color)(tools.logBook)
    moves.find(m => Seq(move, s"$move+").contains(m.show(tools, moves))).map(move => Right(move))
      .getOrElse(Left(
        s"""invalid move $move for $color
           |${tools.chessboard}
           |""".stripMargin))
  }

  private def encode(tools: Tools)(color: Color, moves: Seq[String]): Either[String, Tools] = {
    case class ToolsWithColor(tools: Tools, color: Color)

    moves
      .foldLeft(Right(ToolsWithColor(tools, color = color)): Either[String, ToolsWithColor])(
        (errOrnewToolsWithColor, move) => errOrnewToolsWithColor.fold(
          err => Left(err),
          newToolsWithColor =>
            encode(newToolsWithColor.tools, newToolsWithColor.color)(move).map(genericMove =>
              newToolsWithColor.copy(
                tools = newToolsWithColor.tools.play(genericMove),
                color = newToolsWithColor.color.invert))))
  }.map(_.tools)

  def encodeMultiline(tools: Tools, moves: String): Either[String, Moves] = {
    val (color, genericMoves) = readMultiline(moves)
    encode(tools)(color, genericMoves).map(_.logBook.moves)
  }
}
