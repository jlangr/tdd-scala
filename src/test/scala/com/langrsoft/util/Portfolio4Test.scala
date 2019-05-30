package com.langrsoft.util

import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito}
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}
import com.langrsoft.util.Portfolio4._

class Portfolio4Test extends FunSpec
  with Matchers with BeforeAndAfter
  with IdiomaticMockito
  with ArgumentMatchersSugar
{
  describe("a portfolio") {
    it("is empty for newly created data") {
      isEmpty(PortfolioData()) shouldBe true
    }

    it("has size 0 when created") {
      uniqueSymbolCount(PortfolioData()) shouldBe 0
    }

    it("is no longer empty after purchase") {
      val portfolioData = purchase(PortfolioData(), "BAYN", 10)

      isEmpty(portfolioData) shouldBe false
    }

    it("increases size after purchase") {
      val portfolioData = purchase(PortfolioData(), "BAYN", 10)

      uniqueSymbolCount(portfolioData) shouldBe 1
    }

    it("increments size with each purchase") {
      var portfolioData = purchase(PortfolioData(), "BAYN", 10)
      portfolioData = purchase(portfolioData, "IBM", 10)

      uniqueSymbolCount(portfolioData) shouldBe 2
    }

    it("does not increment size with same symbol purchase") {
      var portfolioData = purchase(PortfolioData(), "BAYN", 10)
      portfolioData = purchase(portfolioData, "BAYN", 10)

      uniqueSymbolCount(portfolioData) shouldBe 1
    }

    it("returns the number of shares purchased") {
      val portfolioData = purchase(PortfolioData(), "BAYN", 17)

      shares(portfolioData, "BAYN") shouldBe 17
    }

    it("returns 0 for symbol not purchased") {
      shares(PortfolioData(), "BAYN") shouldBe 0
    }

    it("adds shares for same symbol purchase") {
      var portfolioData = purchase(PortfolioData(), "BAYN", 42)
      portfolioData = purchase(portfolioData, "BAYN", 10)

      shares(portfolioData, "BAYN") shouldBe 52
    }

    it("returns shares specific to symbol") {
      var portfolioData = purchase(PortfolioData(), "BAYN", 42)
      portfolioData = purchase(portfolioData, "IBM", 10)

      shares(portfolioData, "BAYN") shouldBe 42
    }

    it("throws on purchase non positive shares") {
      an [InvalidPurchaseException] should be thrownBy
        purchase(PortfolioData(), "BAYN", 0)
    }

    describe("value") {
      it("is zero when created") {
        portfolioValue(PortfolioData(), mock[StockService]) shouldBe 0
      }

      it("accumulates prices for all symbols") {
        val BayerPrice = 19
        val IbmPrice = 100

        val service: StockService = mock[StockService]
        service.price("BAYN") shouldReturn(BayerPrice)
        service.price("IBM") shouldReturn(IbmPrice)

        var portfolioData = purchase(PortfolioData(), "BAYN", 10)
        portfolioData = purchase(portfolioData, "IBM", 20)

        portfolioValue(portfolioData, service) shouldBe BayerPrice * 10 + IbmPrice * 20
      }
    }
  }
}
