package com.langrsoft.util

import org.joda.time.{DateTime, Seconds}
import org.mockito.captor.{ArgCaptor, Captor}
import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito, MockitoSugar}
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

class PortfolioTest extends FunSpec
  with Matchers with BeforeAndAfter
  with IdiomaticMockito
  with ArgumentMatchersSugar
{
  val BayerPrice = 19
  val IbmPrice = 19
  var portfolio: Portfolio = _
  val stockService = mock[StockService]

  before {
    portfolio = new Portfolio(stockService)
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

      it("is share value after purchase single share") {
        val stockService = new StockService {
          override def price(symbol: String): Int = BayerPrice
        }

        portfolio.purchase("BAYN", 1)

        portfolio.valueByHand(stockService) shouldBe BayerPrice
      }
    }

    describe("value using mockito") {
      it("accumulates prices for all symbols") {
        val stockService = mock[StockService]
        val portfolio = new Portfolio(stockService)
        stockService.price("BAYN") shouldReturn(BayerPrice)
        stockService.price("IBM") shouldReturn(IbmPrice)

        portfolio.purchase("BAYN", 10)
        portfolio.purchase("IBM", 20)

        portfolio.value shouldBe BayerPrice * 10 + IbmPrice * 20
      }

      it("sets value to 0 on throw") {
        val stockService = mock[StockService]
        val portfolio = new Portfolio(stockService)
        stockService.price("IBM") shouldThrow(new RuntimeException)

        portfolio.purchase("IBM", 20)

        portfolio.value shouldBe 0
      }
    }

    describe("transaction audits") {
      it("audits on purchase") {
        val stockService = mock[StockService]
        val auditor = mock[Auditor]
        val portfolio = new Portfolio(stockService, Some(auditor))

        portfolio.purchase("BAYN", 10)

        auditor.audit("buy: 10 of BAYN", any[DateTime]) was called
      }

      it("checks up on the argument") {
        val stockService = mock[StockService]
        val auditor = mock[Auditor]
        val portfolio = new Portfolio(stockService, Some(auditor))
        val captor = ArgCaptor[DateTime]
        val now = new DateTime

        portfolio.purchase("BAYN", 10)

        auditor.audit("buy: 10 of BAYN", captor) was called
        secondsBetween(captor.value, now) should be < 1
      }

      it("re-throws when auditor throws") {
        val stockService = mock[StockService]
        val auditor = mock[Auditor]
        val portfolio = new Portfolio(stockService, Some(auditor))
        new RuntimeException willBe thrown by auditor.audit(*, *)

        an [AuditException] should be thrownBy portfolio.purchase("", 1)
      }
    }
  }

  private def secondsBetween(before: DateTime, now: DateTime) = {
    Seconds.secondsBetween(now, before).getSeconds
  }
}
