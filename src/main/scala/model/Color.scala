package model

trait Color {
  def invert: Color
}
case object Black extends Color {
  override def invert: Color = White
}
case object White extends Color {
  override def invert: Color = Black
}
