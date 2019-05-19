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
    it("converts numbers using recursion") {
      Roman.convertR(1) shouldBe "I"
      Roman.convertR(2) shouldBe "II"
      Roman.convertR(3) shouldBe "III"
      Roman.convertR(4) shouldBe "IV"
      Roman.convertR(10) shouldBe "X"
      Roman.convertR(11) shouldBe "XI"
      Roman.convertR(20) shouldBe "XX"
      Roman.convertR(50) shouldBe "L"
    }
  }
}
