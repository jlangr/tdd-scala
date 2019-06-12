package com.langrsoft.util

import org.scalatest.{FunSpec, Matchers}

class Roman2Test extends FunSpec with Matchers {
  describe("a roman converter") {
    it("converts arabics") {
      Roman2.convert(1) shouldEqual ("I")
      Roman2.convert(2) shouldEqual ("II")
      Roman2.convert(3) shouldEqual ("III")
      Roman2.convert(4) shouldEqual ("IV")
      Roman2.convert(5) shouldEqual ("V")
      Roman2.convert(9) shouldEqual ("IX")
      Roman2.convert(10) shouldEqual ("X")
      Roman2.convert(11) shouldEqual ("XI")
      Roman2.convert(20) shouldEqual ("XX")
      Roman2.convert(40) shouldEqual ("XL")
      Roman2.convert(50) shouldEqual ("L")
      Roman2.convert(90) shouldEqual ("XC")
      Roman2.convert(300) shouldEqual ("CCC")
      Roman2.convert(304) shouldEqual ("CCCIV")
      Roman2.convert(400) shouldEqual ("CD")
      Roman2.convert(500) shouldEqual ("D")
      Roman2.convert(900) shouldEqual ("CM")
      Roman2.convert(1000) shouldEqual ("M")
    }
  }
}
//  val conversions = Seq((10 -> "X"), (1 -> "I"))
