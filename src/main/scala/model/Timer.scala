package model

trait Timer
case class TimerBlitz(minute: Int = 5) extends Timer
case object Infinite extends Timer