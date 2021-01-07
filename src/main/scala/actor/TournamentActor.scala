package actor

import actor.ChessGameActor.StartClock
import akka.actor.{ Actor, ActorLogging, ActorRef }
import model.Chessboard.EndGame
import model.{ ChessGame, Color, LogBook }

object TournamentActor {
  case class SetGames(game: Seq[ChessGame], arbitrator: ActorRef)
  case object Ready
  case class EndOfGame(
    game: String,
    winner: Option[Color],
    logBook: LogBook,
    cause: EndGame)
}

class TournamentActor extends Actor with ActorLogging {
  import TournamentActor._

  var games: Seq[ActorRef] = Nil

  def receive: PartialFunction[Any, Unit] = {
    case SetGames(games: Seq[ChessGame], arbitrator: ActorRef) =>
      log.debug(s"set ${games.size} game(s)")
      this.games = games.map(game => {
        val chessGameActor = ChessGameActor(game, arbitrator)
        log.debug(s"creating actor ${game.name}")
        context.actorOf(chessGameActor, game.name)
      })

    case Ready =>
      log.debug(s"ready: ${games.size} game(s)")
      log.debug(games.toString())
      this.games.foreach(game => game ! StartClock)

    case EndOfGame(game: String, maybeWinner: Option[Color], logBook: LogBook, endGame) =>
      maybeWinner match {
        case Some(winner) =>
          log.debug(s"end of game: $game. Winner is $winner.")
        case None =>
          log.debug(s"end of game: $game. Game is draw. Cause: ${endGame.toString}")
      }
      log.debug(logBook.toString())
  }
}
