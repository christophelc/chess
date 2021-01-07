package stat

import java.time.{ Clock, LocalDateTime, ZoneOffset }

object Stat {
  def default(): Stat = {
    val time = StatTimeUtil.currentTime()
    Stat(
      timeBegin = time,
      timeEnd = time)
  }
}

case class Stat(
  nCall: Int = 0,
  timeBegin: Long,
  timeEnd: Long,
  nPosition: Long = 0) {

  def show: String = {
    val timeElapsed = (timeEnd - timeBegin) / 1000.0d
    val avg = "%02f".format(nPosition / timeElapsed.toDouble)
    s"pos/s: $avg - nCall: $nCall - time elapsed (s): $timeElapsed - nPosition: $nPosition"
  }
}
