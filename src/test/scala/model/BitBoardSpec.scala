package model

import java.math.BigInteger

import org.specs2.mutable.Specification

class BitBoardSpec extends Specification {

  def binaryToLong(b: String): Long = new BigInteger(b, 2).longValue()

  "min, max and -1L" should {
    "be comparable to binary" in {
      val min = Long.MinValue
      val max = Long.MaxValue
      val minus1 = -1L
      val minBinary = min.toBinaryString
      val maxBinary = max.toBinaryString + "0"
      val minus1Bin = minus1.toBinaryString
      minBinary shouldEqual "1000000000000000000000000000000000000000000000000000000000000000"
      maxBinary shouldEqual "1111111111111111111111111111111111111111111111111111111111111110"
      minus1Bin shouldEqual "1111111111111111111111111111111111111111111111111111111111111111"

      binaryToLong("1000000000000000000000000000000000000000000000000000000000000000") shouldEqual Long.MinValue
    }
  }

  "xxx" should {
    "xxx" in {
      val rookBitboard = binaryToLong("1000000000000000000000000000000000000000000000000000000000000000")
      rookBitboard shouldEqual Long.MinValue
    }
  }
}
