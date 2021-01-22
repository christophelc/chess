package dto

import dto.MoveDtoSpec.game1
import model.board.ChessboardImplConfiguration$Piece
import model.{ LogBook, Tools, White }
import org.specs2.mutable.Specification

class MoveDtoSpec extends Specification {

  "The move 1.e4 - c5" should {
    "be transposed to two moves" in {
      val move = "1.e4 - c5"
      val moveDto = MoveDto.read(move)
      moveDto shouldEqual TwoMovesDto(1, Some("e4"), Some("c5"))
    }
  }

  "The move 1.e4 - " should {
    "be transposed to one move" in {
      val move = "1.e4 -"
      val moveDto = MoveDto.read(move)
      moveDto shouldEqual TwoMovesDto(1, Some("e4"), None)
    }
  }

  "The move 1.- c5" should {
    "be transposed to one move" in {
      val move = "1. - c5"
      val moveDto = MoveDto.read(move)
      moveDto shouldEqual TwoMovesDto(1, None, Some("c5"))
    }
  }

  "Multilines moves" should {
    "be completeliy read" in {
      val (color, moves) = MoveDto.readMultiline(game1)
      color shouldEqual White
      moves.size shouldEqual game1.split("\\n").length * 2
    }
  }

  "Multilines moves" should {
    "be completeliy read" in {
      val tools = Tools(
        chessboard = ChessboardImplConfiguration$Piece(),
        logBook = LogBook())
      val moves = MoveDto.encodeMultiline(tools, game1)
      moves.map(_.size) shouldEqual Right(game1.split("""\n""").length * 2)
    }
  }

}

object MoveDtoSpec {
  val game1: String =
    """1. g3 - h5
      |2. Nh3 - Nc6
      |3. a4 - f6
      |4. c3 - d6
      |5. Ra2 - b6
      |6. Rg1 - Bb7
      |7. Qb3 - a5
      |8. Ra3 - Rb8
      |9. Qb4 - Rh6
      |10. Qg4 - Ne5
      |11. Ra1 - Bd5
      |12. Qd7 - Kf7
      |13. e4 - b5
      |14. Na3 - Rc8
      |15. b3 - Rb8
      |16. Ke2 - Ng6
      |17. Qc8 - Rb7
      |18. Qg4 - h4
      |19. Kd1 - b4
      |20. Qc8 - Qxc8
      |21. Nc4 - Ra7
      |22. d3 - Bc6
      |23. Rg2 - Rh7
      |24. Ra3 - Bb5
      |25. Bg5 - Nh8
      |26. Bxf6 - Qf5
      |27. Be2 - g5
      |28. Bxh8 - Ra6
      |29. Bh5 - Rxh5
      |30. Bf6 - Qxf2
      |31. Nxa5 - Qb6
      |32. Rg1 - Bc4
      |33. Bd4 - Qb8
      |34. Ba7 - bxa3
      |35. Ke2 - Be6
      |36. Bb6 - Rh6
      |37. b4 - Bf5
      |38. Rf1 - Ke8
      |39. Kd1 - Bg4
      |40. Kc1 - Bd1
      |41. Nxg5 - Qc8
      |42. Kxd1 - Rh8
      |43. Ba7 - Nh6
      |44. Bg1 - Bg7
      |45. Nc4 - Rb6
      |46. Nh3 - Nf5
      |47. Rf3 - Rb5
      |48. Be3 - Qa6
      |49. Na5 - Nxe3
      |50. Kd2 - Rd5
      |51. d4 - a2
      |52. Ke1 - Nc2
      |53. Kd1 - e5
      |54. Ng5 - Qxa5
      |55. Ne6 - Na3
      |56. Kd2 - hxg3
      |57. Nf8 - Rh5
      |58. Ke3 - exd4
      |59. Ke2 - Rd5f5
      |60. Kd3 - Rh4
      |61. e5 - Rf5h5
      |62. exd6 - cxd6
      |63. b5 - Qxa4
      |64. Rf2 - a1=B
      |65. hxg3 - Ke7
      |66. Rf6 - Qd1
    |""".stripMargin

  // null by repetition
  val game2: String =
    """1. b4 - d6
      |2. Nf3 - c5
      |3. e3 - h6
      |4. Rg1 - d5
      |5. Ke2 - Qc7
      |6. a4 - b5
      |7. Ne5 - Na6
      |8. Kd3 - bxa4
      |9. c4 - a3
      |10. Kc3 - Bd7
      |11. Nxa3 - Ba4
      |12. e4 - Kd8
      |13. Qe2 - g5
      |14. Nb1 - Qc8
      |15. Ra2 - dxc4
      |16. Qe1 - cxb4
      |17. Kd4 - Qf5
      |18. Nd7 - Bb5
      |19. Qe2 - Qxd7
      |20. Ke5 - Qf5
      |21. Kd4
      |""".stripMargin

  val game3: String =
    """1. d4 - c5
      |2. f3 - c4
      |3. g3 - c3
      |4. h3 - cxb2
      |5. Nd2 - bxa1=R
      |6. e3 - Rxc1
      |7. d5 - Rxd1
      |8. Ke2 - Rxd2
      |9. Kxd2 - Qa5
      |10. Kc1 - Qa3
      |11. Kb1 - Nc6
      |12. h4 - Nb4
      |13. Rh3 - Qxa2
      |14. Kc1 - Qxc2
      |""".stripMargin
}