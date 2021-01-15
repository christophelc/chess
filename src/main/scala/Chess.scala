import java.time.Clock

import actor.StatActor
import actor.TournamentActor.Ready
import akka.actor.ActorRef
import model.Tournament.system
import model._
import model.analyze.{ EngineAlphaBeta, EngineMinMax, EngineRandom }
import stat.{ InterceptActor, InterceptProfiler, Stat }

object Chess extends App {

  val nPlayersInTournament = 2
  val myStatActor: ActorRef = system.actorOf(StatActor(Stat.default()))
  //  val players = for (id <- 0 until nPlayersInTournament) yield PlayerComputer(
  //    id.toString,
  //    new EngineRandom)
  val players = for (id <- 0 until nPlayersInTournament) yield PlayerComputer(
    id.toString,
    if (id == 0)
      new EngineAlphaBeta(depth = 4) with InterceptActor with InterceptProfiler { override val statActor: ActorRef = myStatActor }
    else
      new EngineAlphaBeta(depth = 3) with InterceptActor with InterceptProfiler { override val statActor: ActorRef = myStatActor })

  //  val players = Seq(
  //    PlayerComputer("1", new EngineRandom),
  //    PlayerComputer("2", new EngineAlphaBeta(3) with InterceptActor with InterceptProfiler { override val statActor: ActorRef = myStatActor }))
  //PlayerReal("me"))
  val tournamentActor = Tournament(Arbitrator("arbitrator1"), players, TimerBlitz(), myStatActor)

  tournamentActor ! Ready
  //val nTurn = 10
  //Range(0, nTurn).foldLeft(tournament)((t, _) => t.play())
}

