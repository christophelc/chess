package model.analyze

import akka.actor.ActorRef
import model.{ Color, Tools }
import model.data.Tree
import stat.Intercept

class EngineCheckMate(depth: Int) extends EngineMinMax(depth) {
  self: Intercept =>

  def pruneForCheck(tree: Tree, whichPlayer: Color, currentColor: Color): Tree = {
    val maybePrunedNode = Tree.asNode(if (whichPlayer == currentColor) {
      // It's the turn of the player that has to solve the mat in x moves
      // Prune the moves of the opponent if the opponent is not in check
      val node = Tree.asNode(tree)
      node.copy(children = node.children.filter {
        case (_, node: Tree) => Tree.asNode(node).check
      })
    } else {
      // it's the opponent turn. Try all the move: do not prune any move
      tree
    })
    maybePrunedNode.copy(children = maybePrunedNode.children.map {
      case (move, subtree) => (move, pruneForCheck(
        tree = Tree.asNode(subtree),
        whichPlayer = whichPlayer.invert,
        currentColor = currentColor))
    })
  }

  override def optimize(tools: Tools, color: Color, deep: Int, tree: Tree): Tree = pruneForCheck(tree, color, color)

}
