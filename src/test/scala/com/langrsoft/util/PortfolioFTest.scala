package com.langrsoft.util

import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

class PortfolioFTest extends FunSpec with BeforeAndAfter with Matchers {
  describe("portfolio") {
    var data: PortfolioData = null

    before {
      data = PortfolioData()
    }

    it("is empty when created") {
      PortfolioF.isEmpty(data) shouldBe true
    }

    it("has count 0 when created") {
      PortfolioF.count(data) shouldBe 0
    }

    it("is not empty after purchase") {
      data = PortfolioF.purchase(data, "BAYN", 10)

      PortfolioF.isEmpty(data) shouldBe false
    }

    it("sets count after purchase") {
      data = PortfolioF.purchase(data, "BAYN", 10)

      PortfolioF.count(data) shouldBe 1
    }

    it("increments count with additional purchase") {
      data = PortfolioF.purchase(data, "BAYN", 10)
      data = PortfolioF.purchase(data, "IBM", 20)

      PortfolioF.count(data) shouldBe 2
    }

    it("does not increment count with re-purchase same symbol") {
      data = PortfolioF.purchase(data, "BAYN", 10)
      data = PortfolioF.purchase(data, "BAYN", 20)

      PortfolioF.count(data) shouldBe 1
    }

    it("stores shares with symbol") {
      data = PortfolioF.purchase(data, "BAYN", 10)

      PortfolioF.sharesOf(data, "BAYN") shouldBe 10
    }

    it("returns 0 when symbol not purchased") {
      PortfolioF.sharesOf(data, "BAYN") shouldBe 0
    }

    it("accumulates shares by symbol") {
      data = PortfolioF.purchase(data, "BAYN", 10)
      data = PortfolioF.purchase(data, "BAYN", 20)

      PortfolioF.sharesOf(data, "BAYN") shouldBe 30
    }

    it("throws on purchase negative shares") {
      an [IllegalArgumentException] should be thrownBy PortfolioF.purchase(data, "", -1)
    }
  }
}
