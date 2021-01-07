package actor

import java.time.Clock

import akka.actor.{ Actor, ActorLogging, ActorRef, PoisonPill, Props }
import stat.{ Profiling, Stat, StatTimeUtil }

object StatActor {
  case object ClearStat
  case class UpdateStat(nMove: Int)
  case class UpdateProfiling(functionName: String, timeMilli: Long)

  def apply(stat: Stat): Props = {
    Props(new StatActor(stat = stat))
  }
}

import actor.StatActor._

class StatActor(
  var stat: Stat,
  profile: scala.collection.mutable.Map[String, Profiling] = scala.collection.mutable.Map[String, Profiling]())
  extends Actor with ActorLogging {

  def receive: PartialFunction[Any, Unit] = {
    case ClearStat =>
      profile.clear()
      stat = Stat.default()
    case UpdateStat(nMove: Int) =>
      stat = this.stat.copy(
        nCall = stat.nCall + 1,
        timeEnd = StatTimeUtil.currentTime(),
        nPosition = this.stat.nPosition + nMove)
      if (stat.nCall % 1000 == 0)
        log.debug(stat.show)

    case UpdateProfiling(functionName, dt) =>
      val current = profile
        .getOrElse(functionName, Profiling(functionName, timeElapsedMilli = 0))
      profile.put(functionName, Profiling(
        functionName = functionName,
        nCall = current.nCall + 1,
        timeElapsedMilli = current.timeElapsedMilli + dt))
      if ((current.nCall + 1) % 10000 == 0)
        log.debug(s"$functionName: %02f ms".format(profile(functionName).timeElapsedMilli / profile(functionName).nCall.toDouble) + s" x ${profile(functionName).nCall}")
  }
}
