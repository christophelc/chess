package model

import model.Square._
import org.specs2.mutable.Specification
import model.board.RichSquare._
import model.board.SquareXY

class SquareSpec extends Specification {

  "Square(0, 0)" should {
    "equal A1" in {
      val a = SquareXY(row = row1, col = colA)
      a.toString must equalTo("A1")
    }
  }

  "Square(A1)" should {
    "equal A1" in {
      val a1 = "a1".toSquare
      a1.row must equalTo(row1)
      a1.col must equalTo(colA)
      a1.toString must equalTo("A1")
    }
  }

  "Square(1, 4)" should {
    "equal E2" in {
      val a = SquareXY(row = row2, col = colE)
      a.toString must equalTo("E2")
    }
  }

}
