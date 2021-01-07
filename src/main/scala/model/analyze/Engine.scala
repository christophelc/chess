package model.analyze

import model.{ Color, Tools }
import model.data.Tree
import stat.Intercept

trait Engine {

  def findVariations(
    tools: Tools,
    color: Color): Tree
}
