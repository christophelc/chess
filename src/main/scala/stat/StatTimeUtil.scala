package stat

import java.time.{ Clock, LocalDateTime, ZoneOffset }

object StatTimeUtil {
  def currentTime(): Long = System.currentTimeMillis()
}
