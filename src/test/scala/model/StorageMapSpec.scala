package model

import model.Piece.PieceId
import model.board.{ KingBoard, RookBoard }
import model.board.RichSquare._
import model.data.{ Storage, StorageMap }
import org.specs2.mutable.Specification

class StorageMapSpec extends Specification {

  "Default storage" should {
    "be empty" in {
      val storage: Storage[PieceId, Piece] = StorageMap()
      storage.nonEmpty should beFalse
      storage.isEmpty should beTrue
    }
  }

  "A storage with pieces added" should {
    "not be empty" in {
      val storage: Storage[PieceId, Piece] = StorageMap()
      val king = KingBoard(White, "e1".toSquare)
      val rook = RookBoard(White, "h1".toSquare)
      val s = storage
        .add(king.id)(Seq(king))
        .add(rook.id)(Seq(rook))
      s.isEmpty should beFalse
      s.nonEmpty should beTrue
    }
  }

  "A storage with several pieces added" should {
    "retrieve all the pieces added" in {
      val storage: Storage[PieceId, Piece] = StorageMap()
      val whiteKing = KingBoard(White, "e1".toSquare)
      val whiteRook = RookBoard(White, "h1".toSquare)
      val blackKing = KingBoard(Black, "e1".toSquare)
      val blackRook = RookBoard(Black, "h1".toSquare)
      val s = storage
        .add(whiteKing.id)(Seq(whiteKing))
        .add(whiteRook.id)(Seq(whiteRook))
        .add(blackKing.id)(Seq(blackKing))
        .add(blackRook.id)(Seq(blackRook))
      s.toSeq should containTheSameElementsAs(Seq(
        whiteKing, whiteRook, blackKing, blackRook))
    }
  }

  "A fusion of 2 storages" should {
    "give a new storage with all the elements inside" in {
      val storage: Storage[PieceId, Piece] = StorageMap()
      val whiteKing = KingBoard(White, "e1".toSquare)
      val whiteRook = RookBoard(White, "h1".toSquare)
      val s1 = storage
        .add(whiteKing.id)(Seq(whiteKing))
        .add(whiteRook.id)(Seq(whiteRook))
      val blackKing = KingBoard(Black, "e1".toSquare)
      val blackRook = RookBoard(Black, "h1".toSquare)
      val s2 = storage
        .add(blackKing.id)(Seq(blackKing))
        .add(blackRook.id)(Seq(blackRook))
      s1.toSeq should containTheSameElementsAs(Seq(
        whiteRook, whiteKing))
      s2.toSeq should containTheSameElementsAs(Seq(
        blackRook, blackKing))
      val s = s1.add(s2)
      s.toSeq should containTheSameElementsAs(Seq(
        whiteKing, whiteRook, blackKing, blackRook))
    }
  }

}
