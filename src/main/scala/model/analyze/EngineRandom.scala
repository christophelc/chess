package model.analyze

import model.board.ChessboardImplConfiguration$Piece
import model.data.{ Node, Tree }
import model.{ Color, Tools }

class EngineRandom extends Engine {

  val random = scala.util.Random

  def findVariations(
    tools: Tools,
    color: Color): Tree = {
    val moves = tools.chessboard.generateMove(color)(tools.logBook)
    val root = Node()
    root.copy(children = Seq((moves.toSeq(random.nextInt(moves.countV)), Node(parent = Some(root)))))
  }
}

