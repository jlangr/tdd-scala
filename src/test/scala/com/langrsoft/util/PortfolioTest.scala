package com.langrsoft.util

import org.scalatest.{BeforeAndAfter, FunSpec, ShouldMatchers }

class PortfolioTest extends FunSpec with ShouldMatchers with BeforeAndAfter {
  var normalizer: Portfolio = null

  before {
    normalizer = new Portfolio
  }

  describe("a name normalizer") {
    it("is empty when created") {
      normalizer.isEmpty shouldBe true
    }

    it("has size 0 when created") {
      normalizer.size shouldBe 0
    }

    // TODO: nesting describe / it

    it("is no longer empty after purchase") {
      normalizer.purchase("BAYN", 10)

      normalizer.isEmpty shouldBe false
    }

    it("increases size after purchase") {
      normalizer.purchase("BAYN", 10)

      normalizer.size shouldBe 1
    }

    it("increments size with each purchase") {
      normalizer.purchase("BAYN", 10)
      normalizer.purchase("IBM", 10)

      normalizer.size shouldBe 2
    }

    it("does not increment size with same symbol purchase") {
      normalizer.purchase("BAYN", 10)
      normalizer.purchase("BAYN", 10)

      normalizer.size shouldBe 1
    }

    it("returns the number of shares purchased") {
      normalizer.purchase("BAYN", 42)

      normalizer.shares("BAYN") shouldBe 42
    }

    it("returns 0 for symbol not purchased") {
      normalizer.shares("BAYN") shouldBe 0
    }

    it("adds shares for same symbol purchase") {
      normalizer.purchase("BAYN", 42)
      normalizer.purchase("BAYN", 10)

      normalizer.shares("BAYN") shouldBe 52
    }

    it("throws on purchase non positive shares") {
      an [InvalidPurchaseException] should be thrownBy
        normalizer.purchase("BAYN", 0)
    }
  }
}
