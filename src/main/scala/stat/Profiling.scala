package stat

import actor.StatActor.UpdateProfiling
import akka.actor.ActorRef

object Profiling {
  def time[R](functionName: String)(block: => R)(implicit statActor: ActorRef): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    statActor ! UpdateProfiling(functionName = functionName, timeMilli = t1 - t0)
    result
  }
}

case class Profiling(
  functionName: String,
  nCall: Int = 0,
  timeElapsedMilli: Long)
