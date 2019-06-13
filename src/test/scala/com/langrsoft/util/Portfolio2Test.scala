package com.langrsoft.util

import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito, InjectMocks, Mock, MockitoAnnotations}
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

class Portfolio2Test extends FunSpec
  with Matchers with BeforeAndAfter
  with IdiomaticMockito
  with ArgumentMatchersSugar
{
  val BayerPrice = 19
  val IbmPrice = 100

  @InjectMocks
  var portfolio: Portfolio2 = _
  @Mock
  var stockService: StockService = _

  before {
    portfolio = new Portfolio2
    MockitoAnnotations.initMocks(this)
  }

  describe("a portfolio") {
    it("is empty when created") {
      portfolio.isEmpty shouldBe true
    }

    it("has size 0 when created") {
      portfolio.size shouldBe 0
    }

    it("is no longer empty after purchase") {
      portfolio.purchase("BAYN", 10)

      portfolio.isEmpty shouldBe false
    }

    it("increases size after purchase") {
      portfolio.purchase("BAYN", 10)

      portfolio.size shouldBe 1
    }

    it("increments size with each purchase") {
      portfolio.purchase("BAYN", 10)
      portfolio.purchase("IBM", 10)

      portfolio.size shouldBe 2
    }

    it("does not increment size with same symbol purchase") {
      portfolio.purchase("BAYN", 10)
      portfolio.purchase("BAYN", 10)

      portfolio.size shouldBe 1
    }

    it("returns the number of shares purchased") {
      portfolio.purchase("BAYN", 42)

      portfolio.shares("BAYN") shouldBe 42
    }

    it("returns 0 for symbol not purchased") {
      portfolio.shares("BAYN") shouldBe 0
    }

    it("adds shares for same symbol purchase") {
      portfolio.purchase("BAYN", 42)
      portfolio.purchase("BAYN", 10)

      portfolio.shares("BAYN") shouldBe 52
    }

    it("throws on purchase non positive shares") {
      an [InvalidPurchaseException] should be thrownBy
        portfolio.purchase("BAYN", 0)
    }

    describe("value") {
      it("is zero when created") {
        portfolio.value shouldBe 0
      }

      it("accumulates prices for all symbols") {
        stockService.price("BAYN") shouldReturn(BayerPrice)
        stockService.price("IBM") shouldReturn(IbmPrice)

        portfolio.purchase("BAYN", 10)
        portfolio.purchase("IBM", 20)

        portfolio.value shouldBe BayerPrice * 10 + IbmPrice * 20
      }
    }
  }
}