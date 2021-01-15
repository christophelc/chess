package model.analyze

import actor.StatActor.UpdateStat
import model.board.ChessboardImpl
import model.data.Tree._
import model.data._
import model.{ Color, GenericMove, Tools }
import stat.Intercept

class EngineMinMax(depth: Int) extends Engine {
  self: Intercept =>

  def generateTree(
    tools: Tools,
    currentColor: Color,
    deep: Int = 1,
    tree: Tree): Tree = {

    def play(move: GenericMove): Tools = {
      profile("play") {
        tools.playAndUpdateControls(move)
      }
    }

    def generateChildrenAndMoves: Children = {
      profile("generateChildrenAndMoves") {
        val moves = tools.chessboard.generateMove(currentColor)(tools.logBook)
        sendStat(UpdateStat(moves.countM))
        (for (move <- moves.toSeq) yield move -> Node(
          parent = Some(tree)))
      }
    }

    def updateScoreLeaf(tree: Tree, score: Int, check: Boolean): Tree =
      Tree.asNode(tree).copy(
        score = Some(score),
        check = check)

    def childEvaluate(tree: Tree): Tree = {
      val node = Tree.asNode(tree)
      val check = tools.chessboard.isCheck(currentColor)
      if (deep == 0) {
        updateScoreLeaf(
          tree,
          (Score.evaluate(currentColor.invert, tools)
            - Score.scoreCapture(tools.chessboard, tools.logBook.moves.last)),
          check)
      } else {
        // if no child, generate new one
        val children = if (node.children.isEmpty) {
          generateChildrenAndMoves
        } else {
          node.children
        }
        if (children.isEmpty) {
          node.copy(
            score = if (check) Some(Int.MaxValue) else Some(0),
            check = check)
        } else {
          // iterate over children and update the global score of the node
          val updateChildren = (for ((move, subtree) <- children) yield (move -> {
            val newTools = play(move)
            generateTree(
              newTools,
              currentColor.invert,
              deep - 1,
              subtree)
            // FIXME getOrElse
          })).sortBy { case (_, node: Node) => -node.score.getOrElse(0) }
          node.copy(
            children = updateChildren,
            score = Option(updateChildren.map { case (_, node: Node) => node.score.getOrElse(Int.MinValue) }.max *
              (-1)),
            check = check)
        }
      }
    }
    childEvaluate(tree)
  }

  def optimize(tools: Tools, color: Color, deep: Int, tree: Tree): Tree = tree

  override def findVariations(
    tools: Tools,
    color: Color): Tree = {

    // depth iteration
    Range(2, depth + 1).foldLeft(generateTree(tools, color, deep = 2, Root))(
      (tree, level) => {
        val opt = generateTree(tools, color, deep = level, optimize(tools, color, level, tree))
        println(s"depth = $level -> ${opt.findBestVariation.map(_.showEasy()).mkString(" ")}")
        opt
      })
  }
}
