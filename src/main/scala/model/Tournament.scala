package model

import actor.TournamentActor
import actor.TournamentActor.SetGames
import akka.actor.{ ActorRef, ActorSystem, Props }

object Tournament {
  val system = ActorSystem("games")

  def apply(arbitrator: Arbitrator, players: Seq[Player], timer: Timer): ActorRef = {
    require(players.length % 2 == 0)

    val tournamentActor = system.actorOf(Props[TournamentActor](), "arbitratorActor")

    val games: Seq[ChessGame] = players.grouped(2).map {
      case Seq(playerWhite, playerBlack) => ChessGame(
        playerBlack = playerBlack,
        playerWhite = playerWhite,
        timer = timer)
    }.toSeq
    tournamentActor ! SetGames(games, tournamentActor)
    tournamentActor
  }
}

