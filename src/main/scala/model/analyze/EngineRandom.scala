package model.analyze

import model.board.ChessboardImpl
import model.data.{ Node, Tree }
import model.{ Color, Tools }

class EngineRandom extends Engine {

  val random = scala.util.Random

  def findVariations(
    tools: Tools,
    color: Color): Tree = {
    val moves = ChessboardImpl.convert(tools.chessboard).generateMove(color)(tools.logBook)
    val root = Node()
    root.copy(children = Seq((moves(random.nextInt(moves.length)), Node(parent = Some(root)))))
  }
}

