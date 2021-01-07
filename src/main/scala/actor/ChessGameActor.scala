package actor

import actor.StatActor.ClearStat
import actor.TournamentActor.EndOfGame
import akka.actor.{ Actor, ActorLogging, ActorRef, PoisonPill, Props }
import model.Chessboard.EndGame
import model._

import scala.concurrent.duration.DurationInt

object ChessGameActor {
  case object StartClock
  case class PressClock(color: Color)
  case class Play(color: Color)
  case class GameOver(endGame: EndGame)

  def apply(chessGame: ChessGame, arbitrator: ActorRef, statActor: ActorRef): Props =
    Props(new ChessGameActor(chessGame, arbitrator, statActor))
}

class ChessGameActor(var game: ChessGame, arbitrator: ActorRef, statActor: ActorRef) extends Actor with ActorLogging {
  import ChessGameActor._

  def receive: PartialFunction[Any, Unit] = {

    case StartClock =>
      log.debug(s"The black player ${game.playerColor(Black)} started the clock. White to play")
      self ! Play(White)

    case PressClock(whichPlayer: Color) =>
      if (game.whichPlayerTurn == whichPlayer) {
        log.debug(s"The $whichPlayer player stopped its clock. It's the turn of the ${whichPlayer.invert} to play")
        Thread.sleep(10)
        game = game.copy(whichPlayerTurn = whichPlayer.invert)
        self ! Play(whichPlayer.invert)
      } else {
        log.debug(s"The $whichPlayer cannot stop its clock since it's not its turn.")
      }
    case GameOver(endGame: EndGame) =>
      log.debug(s"game over for game ${game.name}")
      val maybeWinner = if (game.tools.chessboard.isCheck(game.whichPlayerTurn)) {
        Some(game.whichPlayerTurn.invert)
      } else
        None
      arbitrator ! EndOfGame(game.name, maybeWinner, game.tools.logBook, endGame)
      self ! PoisonPill

    case Play(whichPlayer: Color) =>
      game = game.play(whichPlayer)
      statActor ! ClearStat
      game.tools.chessboard.endGame match {
        case Some(endGame) =>
          self ! GameOver(endGame)
        case None =>
          log.debug(game.tools.logBook.moves.last.showEasy())
          log.debug(game.tools.chessboard.toString)
          self ! PressClock(whichPlayer)
      }
  }
}
