package model

import model.Chessboard._
import model.analyze.Engine
import model.board.{ BaseMove, ChessboardImpl }

abstract class Player(val name: String) {
  def findMove(tools: Tools, color: Color): Option[GenericMove]

  def play(tools: Tools, color: Color): Tools = {
    lazy val isInsufficientMaterial = tools.chessboard.isNullByInsufficientMaterial
    lazy val is50MovesRule = tools.logBook.isNull50movesRule
    lazy val isRepetition3x = false // TODO // tools.logBook.isNullByRepetition3x
    Seq(
      (isInsufficientMaterial, EndGameByInsufficientMaterial),
      (is50MovesRule, EndGame50MoveNoTakenPieceNoPawnMove),
      (isRepetition3x, EndGameByRepetition))
      .view
      .find(_._1 == true)
      .map(_._2) match {
        case endGame @ Some(_) =>
          tools.copy(chessboard = tools.chessboard.withEndGame(endGame = endGame))
        case None =>
          val chessboard: Chessboard = tools.chessboard.updateControls(tools.logBook)
          val moves = chessboard.generateMove(color)(tools.logBook)
          if (moves.isEmpty) {
            tools.copy(chessboard = chessboard.withEndGame(Some(EndgameByCheckPat)))
          } else {
            val maybeMove = findMove(tools, color)
            // having no move available may happen for a manual entered position
            maybeMove.map(move => tools.play(move)).getOrElse(tools)
          }
      }
  }
}

case class PlayerComputer(override val name: String, engine: Engine) extends Player(name) {
  override def findMove(tools: Tools, color: Color): Option[GenericMove] = {
    val tree = engine.findVariations(
      tools = tools,
      color = color)
    println(BaseMove.showEasy(tree.findBestVariation))
    tree.findBestMoveFromTree
  }
}
case class PlayerReal(override val name: String) extends Player(name) {
  override def findMove(tools: Tools, color: Color): Option[GenericMove] = {
    import scala.io.StdIn.readLine

    val moves = tools.chessboard.generateMove(color)(tools.logBook)
    def readMove(): GenericMove = {
      var validMove: Option[GenericMove] = None
      do {
        println("Enter your move:")
        println(moves.toSeq.map(_.show(tools, moves).mkString(" ")))
        val move = readLine()
        validMove = moves.toSeq.find(m => Seq(move, s"$move+").contains(m.show(tools, moves)))
      } while (validMove.isEmpty)
      validMove.get
    }
    Some(readMove())
  }
}
