package model.data

import model.GenericMove
import model.board.BaseMove.Moves
import model.data.Tree.Children
object Tree {
  type Children = Seq[(GenericMove, Tree)]

  val Root: Tree = Node()
  def asNode(tree: Tree): Node = tree match { case node: Node => node }
}
sealed trait Tree {
  def show(moveParent: String = ""): String
  def findBestMoveFromTree: Option[GenericMove]
  def findBestVariation: Moves
}
final case class Node(
  parent: Option[Tree] = None,
  score: Option[Int] = None,
  check: Boolean = false,
  children: Children = Seq()) extends Tree {

  override def findBestMoveFromTree: Option[GenericMove] = {
    val movesWithScores = children.map {
      case (move, node2: Node) => (move, node2.score)
    }
    if (movesWithScores.isEmpty)
      None
    else
      Some(movesWithScores.maxBy(_._2)._1)
  }

  override def findBestVariation: Moves =
    findBestMoveFromTree.map(move =>
      move +: children.find(_._1 == move)
        .head._2.findBestVariation).getOrElse(Nil)

  override def show(moveParent: String = ""): String = {
    if (children.isEmpty)
      moveParent + " : " + score.map(_.toString()).getOrElse(" - ")
    else {
      val sCheck = if (check) "+" else ""
      (for ((move, tree) <- children) yield "(" + tree.show(moveParent + " " + move.showEasy() + sCheck) + "): " + score.map(_.toString).getOrElse(" - ")).mkString("\t")
    }
  }
}
