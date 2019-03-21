package com.langrsoft.util

import org.scalatest.{BeforeAndAfter, FunSpec, ShouldMatchers}

class RomanTest extends FunSpec with ShouldMatchers with BeforeAndAfter {
  describe("a roman number converter") {
    it("converts numbers") {
      Roman.convert(1) shouldBe "I"
      Roman.convert(2) shouldBe "II"
      Roman.convert(3) shouldBe "III"
      Roman.convert(10) shouldBe "X"
      Roman.convert(11) shouldBe "XI"
      Roman.convert(20) shouldBe "XX"
      Roman.convert(50) shouldBe "L"
    }
  }
}
