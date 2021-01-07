package model

import org.specs2.mutable.Specification
import RichSquare._

class SquareSpec extends Specification {

  "Square(0, 0)" should {
    "equal A1" in {
      val a = SquareXY(row = 0, col = 0)
      a.toString must equalTo("A1")
    }
  }

  "Square(A1)" should {
    "equal A1" in {
      val a1 = "a1".toSquare
      a1.row must equalTo(0)
      a1.col must equalTo(0)
      a1.toString must equalTo("A1")
    }
  }

  "Square(1, 4)" should {
    "equal E2" in {
      val a = SquareXY(row = 1, col = 4)
      a.toString must equalTo("E2")
    }
  }

}
