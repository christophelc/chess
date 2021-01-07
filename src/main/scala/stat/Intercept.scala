package stat

import actor.StatActor.UpdateStat
import akka.actor.ActorRef

trait Intercept {

  def sendStat(updateState: UpdateStat): Unit = {}
  def profile[R](functionName: String)(block: => R): R = block
}

trait InterceptProfiler extends Intercept {
  val statActor: ActorRef

  override def profile[R](functionName: String)(block: => R): R =
    Profiling.time[R](functionName)(block)(statActor)
}

trait InterceptActor extends Intercept {
  val statActor: ActorRef

  override def sendStat(updateState: UpdateStat): Unit = {
    statActor ! updateState
  }
}