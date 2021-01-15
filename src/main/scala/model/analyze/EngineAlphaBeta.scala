package model.analyze

import akka.actor.ActorRef
import model.{ Color, Tools }
import model.data.{ Node, Tree }
import model.data.Tree.{ Children, Root }
import stat.Intercept

class EngineAlphaBeta(depth: Int) extends EngineMinMax(depth) {
  self: Intercept =>

  def alphabeta(
    tree: Tree,
    alpha: Int = Int.MinValue,
    beta: Int = Int.MinValue): Tree = {

    case class Acc(
      children: Children = Nil,
      skip: Boolean = false,
      alpha: Int = alpha,
      beta: Int = beta)

    val node = Tree.asNode(tree)
    node.copy(children = node.children.foldLeft(Acc())((acc, subtree) => {
      if (acc.skip)
        acc
      else {
        val subtreeNode = Tree.asNode(alphabeta(subtree._2, beta = acc.beta))
        // beta pruning
        if (subtreeNode.score.getOrElse(Int.MaxValue) < acc.beta) {
          acc.copy(skip = true)
        } else {
          val alpha: Int = subtreeNode.score.getOrElse(Int.MinValue)
          // alpha pruning
          if (-alpha < acc.beta) {
            acc.copy(beta = -alpha, skip = true)
          } else {
            // https://www.di.ens.fr/~granboul/enseignement/mmfai/algo2003-2004/tp5/
            // keep the child
            acc.copy(
              alpha = if (alpha > acc.alpha) alpha else acc.alpha,
              beta = -alpha,
              children = acc.children :+ subtree)
          }
        }
      }
    }).children)
  }

  override def optimize(tools: Tools, color: Color, deep: Int, tree: Tree): Tree = alphabeta(tree)

}
