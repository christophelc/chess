package model.board

import dto.{ ChessboardDto, FENCodec }
import model.{ Black, Piece, Pieces, Storage, White }
import model.Piece.PieceId
import model.board._
import model.board.RichSquare._
import org.specs2.mutable.Specification

class PiecesSeqSpec extends Specification {

  "An empty PiecesSeq with pieces added" should {
    "not be empty" in {

      val king = KingBoardImpl(White, "e1".toSquare)
      val pieces: Pieces = PiecesSeq.build(Seq(king))
      pieces.nonEmpty should beTrue
    }
  }

  "PiecesSeq containing a rook and 2 kings" should {
    "be able to find the kings and the rook" in {

      val whiteKing = KingBoardImpl(White, "e1".toSquare)
      val rook = RookBoardImpl(White, "h1".toSquare)
      val blackKing = KingBoardImpl(Black, "e8".toSquare)
      val pieces: Pieces = PiecesSeq.build(Seq(whiteKing, blackKing, rook))
      pieces.king(White) shouldEqual whiteKing
      pieces.king(Black) shouldEqual blackKing
      pieces.rooks shouldEqual PiecesSeq.build(Seq(rook))
      pieces.rooks.toSeq shouldEqual Seq(rook)
    }
  }

  "A fusion of 2 non empty PiecesSeq" should {
    "give a new PiecesSeq containing the pieces" in {

      val whiteKing = KingBoardImpl(White, "e1".toSquare)
      val whiteRook = RookBoardImpl(White, "h1".toSquare)
      val piecesWhite: Pieces = PiecesSeq.build(Seq(whiteKing, whiteRook))
      val blackKing = KingBoardImpl(Black, "e8".toSquare)
      val blackRook = RookBoardImpl(White, "h8".toSquare)
      val piecesBlack: Pieces = PiecesSeq.build(Seq(blackKing, blackRook))
      val r = piecesWhite.union(piecesBlack)
      r.rooks.toSeq should containTheSameElementsAs(Seq(whiteRook, blackRook))
      r.king(White) shouldEqual whiteKing
      r.king(Black) shouldEqual blackKing
    }
  }

}